module Draglist exposing (..)

import Html exposing (Html, button, div, text, Attribute)
import Html.App
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions, Options)
import Mouse exposing (Position)
import Json.Decode as Json exposing ((:=))
import Result
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import List exposing (indexedMap, drop, repeat, map, map2, head, take)
import DOM exposing (target, parentElement, childNodes, offsetTop, offsetHeight)

type Msg e
  = DragStart Int (List Int) Position
  | DragAt Position
  | DragEnd Position
  | ElementMsg Int e

type alias Dragging =
  { index : Int
  , position : Position
  , originalPosition : Position
  , siblingPositions : List Int
  }

type alias Model m =
  { items : List m
  , dragging : Maybe Dragging
  }

init items = (Model items Nothing, Cmd.none)

update : (e -> m -> (m, Cmd e)) -> (Int -> Int -> List m -> Cmd e) -> Msg e -> Model m -> ( Model m, Cmd (Msg e) )
update updateItem repositionCommand msg { items, dragging } =
  let
    doNothing = (Model items Nothing, Cmd.none)
    reorder from to =
      let
        newItems = reposition from to items
        command = repositionCommand from to newItems
      in
        ( Model newItems Nothing
        , Cmd.map (ElementMsg to) command
        )
  in case msg of
    DragStart n vs xy -> (Model items (Just (Dragging n xy xy vs)), Cmd.none)
    DragAt xy -> case dragging of
      Nothing -> doNothing
      Just dragging -> (Model items (Just { dragging | position= xy }), Cmd.none)
    DragEnd xy -> case dragging of
      Nothing -> doNothing
      Just { index, position, originalPosition, siblingPositions } -> let
        y = xy.y
        y0 = originalPosition.y
        droppedAt = indexAtWhich (\p -> y - y0 <= p) siblingPositions
        in if droppedAt < index
          then reorder index droppedAt -- up
          else if index < droppedAt - 1
            then reorder index (droppedAt - 1) -- down
            else doNothing -- it was put back in original position
    ElementMsg n m -> case drop n items of
      [] -> (Model items dragging, Cmd.none)
      itemN :: restItems -> let
        (newItem, msgItem) = updateItem m itemN
        newItems = take n items ++ (newItem :: restItems)
        newMsg = Cmd.map (ElementMsg n) msgItem
        in
          (Model newItems dragging, newMsg)

(=>) = (,)

viewItemContainer : (m -> Html e) -> Maybe Dragging -> Int -> m -> Html (Msg e)
viewItemContainer viewItem dragging index item =
  let pos = case dragging of
    Nothing -> Position 0 0
    Just d -> if index == d.index
      then relative d.position d.originalPosition
      else Position 0 0
  in
    div
    [ onNoBubble "mousedown" (getDragStart index)
    , style
      [ "cursor" => "move"
      , "position" => "relative"
      , "left" => px pos.x
      , "top" => px pos.y
      ]
    ]
    [ Html.App.map (ElementMsg index) (viewItem item) ]

view : (m -> Html e) -> Model m -> Html (Msg e)
view viewItem model = div
  [ Html.Attributes.id "draglist" ]
  (indexedMap (viewItemContainer viewItem model.dragging) model.items)

subscriptions : (m -> Sub e) -> Model m -> Sub (Msg e)
subscriptions itemSubs model = let
  getSub n x = Sub.map (ElementMsg n) (itemSubs x)
  subs = indexedMap getSub model.items
  in case model.dragging of
    Nothing -> Sub.batch subs
    Just _ -> Sub.batch <| Mouse.moves DragAt :: Mouse.ups DragEnd :: subs

relative : Position -> Position -> Position
relative a b = Position (a.x - b.x) (a.y - b.y)

px : Int -> String
px n = toString n ++ "px"

getDragStart index = Json.object2 (getSiblingOffsets index) getSiblingPositions Mouse.position

-- Getting the id should succeed even if there isn't actually an id member.
-- This helps testing, as we don't have to put ids in everywhere.
getId = Json.oneOf
  [ "id" := Json.string
  , Json.succeed ""
  ]

-- find ancestor #draglist
draglistAncestor : Json.Decoder a -> Json.Decoder a
draglistAncestor cont = getId `Json.andThen` \actualId ->
    if actualId == "draglist"
      then cont
      else parentElement (draglistAncestor cont)

-- get the y values of the elements' centres
getSiblingPositions : Json.Decoder (List Int)
getSiblingPositions = target
  <| draglistAncestor
  <| childNodes (Json.object2 (\t h -> round (t + h/2)) offsetTop offsetHeight)

-- how far we need to drag the item we have grabbed to put it into each
-- potential new position
getSiblingOffsets : Int -> List Int -> Position -> (Msg e)
getSiblingOffsets index offsets = case getAt index offsets of
  Nothing -> DragStart index (map (const 0) offsets) -- really this is an error path
  Just y -> DragStart index (map (subtract y) offsets)

onNoBubble name = onWithOptions name (Options True True)

const : a -> b -> a
const x y = x

getAt : Int -> List a -> Maybe a
getAt n xs = drop n xs |> head

subtract a b = b - a

indexAtWhich : (a -> Bool) -> List a -> Int
indexAtWhich pred xs =
  let iaw n xs = case xs of
    [] -> n
    (x::xs') -> if pred x
      then n
      else iaw (n+1) xs'
  in
    iaw 0 xs

insert : Int -> a -> List a -> List a
insert n y xs = if n == 0 then y::xs else case xs of
  [] -> [y]
  x::xs' -> x::(insert (n-1) y xs')

-- returns (nth element of xs) :: (xs without nth element)
bringToHead : Int -> List a -> List a
bringToHead n xs = case drop n xs of
  [] -> xs
  x::after -> x :: take n xs ++ after

reposition : Int -> Int -> List a -> List a
reposition from to xs = if to == 0 then bringToHead from xs
  else case xs of
  [] -> []
  x::xs' -> if from == 0
    then insert to x xs'
    else x :: reposition (from - 1) (to - 1) xs'
