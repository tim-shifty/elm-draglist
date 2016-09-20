module Tests exposing (all)

import Test exposing (Test, describe, test, fuzz2, fuzz3, fuzz4)
import Fuzz exposing (Fuzzer, list, int, intRange, tuple)
import Expect
import String
import List exposing (head, take, drop, length, sort, filter, map, indexedMap, repeat)
import Mouse exposing (Position)
import Json.Decode as Json
import Debug exposing (log)

import Draglist exposing (reposition, indexAtWhich, Dragging,
  getDragStart, const, update, init, Msg (..))

infixl 0 ===
(===) a b = Expect.equal b a

-- fuzzer producing natural numbers
nat : Fuzzer Int
nat = Fuzz.map abs int

nat20 = intRange 0 20

-- fuzzer randomly producing one of x :: xs
oneOf : a -> List a -> Fuzzer a
oneOf x xs = let
  getIt n = case drop n (x::xs) of
    y::_ -> y
    [] -> x
  in Fuzz.map getIt <| intRange 0 (length xs)

-- fuzzer producing a random order relation
ordering : Fuzzer (comparable -> comparable -> Bool)
ordering = oneOf (==) [(/=), (<), (>), (<=), (>=)]

-- returns xs with the nth element missing (or xs if n is not
-- within the range 0 .. lenth xs -1)
delete n xs = case xs of
  [] -> []
  x::xs' -> if n == 0
  then xs'
  else x :: delete (n-1) xs'

-- checks that n is within the range [0..length xs-1]
inRange : Int -> List a -> Bool
inRange n xs = 0 <= n && n < length xs

-- check that all elements of the shorter list are equal to the corresponding
-- elements of the longer list
allEqual : List a -> List a -> Bool
allEqual xs ys = List.all identity <| List.map2 (==) xs ys

all : Test
all = describe "draglist"
  [ describe "delete"
    [ fuzz2 int (list int) "deleting leaves first n alone"
      <| \n xs -> take n (delete n xs)
      === take n xs
    , fuzz2 int (list int) "deleting pushes those after n forward one"
      <| \n xs -> drop n (delete n xs)
      === drop (n+1) xs
    ]
  , describe "reposition"
    [ fuzz3 int int (list int) "repositioning does not change the length of a list"
      <| \from to xs -> length (reposition from to xs)
      === length xs
    , fuzz3 int int (list int) "repositioning puts the nth element in the mth place"
      <| \n m xs -> if inRange n xs && inRange m xs
          then take 1 (drop n xs) === take 1 (drop m (reposition n m xs))
          else sort (reposition n m xs) === sort xs
    , fuzz3 int int (list int) "repositioning leaves all other elements alone"
      <| \n m xs -> if inRange n xs && inRange m xs
          then delete n xs === delete m (reposition n m xs)
          else sort (reposition n m xs) === sort xs
    ]
  , describe "indexAtWhich"
    [ fuzz3 ordering int (list int) "When something matches it returns the first one, otherwise length"
      <| \p v xs -> let pred = p v in case filter pred xs of
        [] -> indexAtWhich pred xs === length xs
        x::_ -> case drop (indexAtWhich pred xs) xs of
          [] -> Expect.fail "returned length or more when an example could be found"
          y::_ -> y === x
    ]
  , describe "getDragStart"
    [ fuzz4 int int nat (list (tuple (int,nat))) "sets up Dragging correctly"
      <| \mouseX mouseY index topsAndHeights ->
            let
              childNode n (top,height) = "\"" ++ toString n ++ "\":{\"offsetTop\":"
              ++ toString top ++ ",\"offsetHeight\":" ++toString height ++ "}"
              mids = case head (drop index topsAndHeights) of
                Nothing -> map (const 0) topsAndHeights
                Just (t0,h0) -> (map (\(t,h)->t+(h+1)//2 - t0 - (h0+1)//2) topsAndHeights)
            in
              Json.decodeString (getDragStart index)
                ( "{\"pageX\":" ++ toString mouseX
                ++ ",\"pageY\":" ++ toString mouseY
                ++ ",\"target\":{"
                  ++ "\"parentElement\":{\"id\":\"draglist\",\"childNodes\":{"
                    ++ String.join "," (indexedMap childNode topsAndHeights)
                    ++ "}}}}" )
                === Result.Ok (Draglist.DragStart
                  index mids (Position mouseX mouseY))
    ]
  , describe "update"
    [ fuzz4 nat20 nat20 nat20 int "update moves element up"
      <| \countBefore moveUp countAfter randomY ->
        let
          totalCount = countBefore + 1 + moveUp + countAfter
          items = repeat (countBefore + moveUp) "before"
            ++ ("this" :: repeat countAfter "after")
          expectedItems = repeat countBefore "before"
            ++ ("this" :: repeat moveUp "before")
            ++ repeat countAfter "after"
          indexToY n = n * 10 + 5
          indexToPosition n = Position 5 <| indexToY n
          mids = map indexToY [0..totalCount-1]
          startPosition = indexToPosition (countBefore + moveUp)
          relativeMids = map (\y -> y - startPosition.y) mids
          startMsg = DragStart (countBefore + moveUp) relativeMids startPosition
          dragMsg = DragAt (Position 5 randomY)
          endMsg = DragEnd (indexToPosition countBefore)
          (model, _) = init items
          updateItem _ m = (m,Cmd.none)
          doUpdate msg model = let
            (m, _) = update updateItem (\_ _ _ -> Cmd.none) msg model
            in m
          (expectedModel, _) = init expectedItems
          actualModel = model
            |> doUpdate startMsg
            |> doUpdate dragMsg
            |> doUpdate endMsg
        in
          expectedModel === actualModel
    ]
  ]
