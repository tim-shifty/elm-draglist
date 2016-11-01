module RealDom exposing (render, query, textContent, classNames, attributes, DomNode)

import Html exposing (Html)
import Set exposing (Set)
import Dict exposing (Dict)
import String exposing (words)

import Native.RealDom

-- not its real type
type DomNode = DomNodeDummyConstructor

render : Html a -> DomNode
render = Native.RealDom.render

query : String -> DomNode -> DomNode
query = Native.RealDom.query

textContent : DomNode -> String
textContent = Native.RealDom.textContent

classNames : DomNode -> Set String
classNames node = node
  |> Native.RealDom.className
  |> words
  |> Set.fromList

attributes : DomNode -> Dict String String
attributes node = node
  |> Native.RealDom.attributes
  |> Dict.fromList
