port module Main exposing (main)

-- Target open layer clusters as Australian place names
-- Control Panel for open layers map
-- Thomas Paine, 2020

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as E
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Http
import Dict exposing (Dict)

port subTarget : (String -> msg) -> Sub msg

main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Msg = HoverJson String
         | DropLocation String
         | JsonResponse (Result Http.Error (List Properties))

type Level = Suburb | Town | Council | City | State
type Scope = Area | Population | PopDensity
type alias Boundaries =
    { ageBounds : (Float, Float)
    , popDenseBounds : (Float, Float)
    }

type alias Model =
    { level : Maybe Level
    , targets : List Properties
    , unfiltered : List Properties
    , filtered : Dict String Int
    , boundaries : Boundaries
    }

type alias Properties =
    { suburb : String
    , town : String
    , council : String
    , city : String
    , state : String
    , area : Float
    , population : Int
    , age : Float
    }

pushBoundaries : Model -> Model
pushBoundaries model = let checkBounds = []
                           boundaries = List.foldl (\prop -> \bounds ->
                                                    let pma = prop.age
                                                        ppd = toFloat prop.population/prop.area
                                                        (aMin, aMax) = bounds.ageBounds
                                                        (bMin, bMax) = bounds.popDenseBounds
                                                    in Boundaries
                                                        (if pma < aMin then pma else aMin, if pma > aMax then pma else aMax)
                                                        (if ppd < bMin then ppd else bMax, if ppd > bMax then ppd else bMax)
                                                   ) model.boundaries model.unfiltered
                       in { model | boundaries = boundaries }

getLocations : Model -> Model
getLocations model = model

init : Int -> (Model, Cmd Msg)
init date = (Model Nothing [] [] Dict.empty  (Boundaries (100,0) (100,0))
         , Http.get
              { url = "geo.json?" ++ String.fromInt date
              , expect = Http.expectJson JsonResponse geoDecoder
              }
         )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
                       HoverJson json -> let (level, targets) = decodeHoverJson json
                                         in ({ model
                                              | level = level
                                              , targets = List.filter (levelFilter (Maybe.withDefault Suburb level) targets) model.unfiltered
                                             }, Cmd.none)
                       DropLocation location -> let locations = List.filter (\v -> levelFunction (Maybe.withDefault Suburb model.level) v /= location) model.targets -- Remove location by filtering
                                                in ({model
                                                        | targets = locations
                                                    } |> \m -> if List.length locations == 0
                                                               then { m | level = Nothing } -- Wipe level if locations is empty
                                                               else m
                                                   , Cmd.none)
                       JsonResponse r -> ({ model
                                              | unfiltered = Result.withDefault [] r
                                          } |> pushBoundaries |> getLocations
                                         , Cmd.none)

geoDecoder : D.Decoder (List Properties)
geoDecoder = D.field "features"
             <| D.list <| D.field "properties"
                            (D.succeed Properties
                                 |> DP.required "sa2" D.string
                                 |> DP.required "sa3" D.string
                                 |> DP.required "sa4" D.string
                                 |> DP.required "sa5" D.string
                                 |> DP.required "sa6" D.string
                                 |> DP.required "area" D.float
                                 |> DP.required "population" D.int
                                 |> DP.optional "age" D.float 0
                            )

decodeHoverJson : String -> (Maybe Level, List String)
decodeHoverJson json = Result.withDefault (Nothing, []) <|
                       D.decodeString (D.map2 (\a -> \b -> (a, b))
                                           (D.field "level" <| D.maybe decodeLevel)
                                           (D.field "locations" <| D.list D.string)
                                      ) json

decodeLevel : D.Decoder Level
decodeLevel = D.map (\str -> case str of
                                 "sa2" -> Suburb
                                 "sa3" -> Town
                                 "sa4" -> Council
                                 "sa5" -> City
                                 "sa6" -> State
                                 _ -> Suburb
                    ) D.string

refilter : Model -> Model
refilter model = model

subscriptions : Model -> Sub Msg
subscriptions model = subTarget HoverJson

jsonEncode : Dict String Int -> String
jsonEncode vs = E.encode 0 <| E.object <| Dict.toList <| Dict.map (\_ -> E.int) vs

view : Model -> Html Msg
view model = div []
                 [ div [id "filtered_json", attribute "data-json" <| jsonEncode model.filtered]
                       [ Maybe.withDefault (div [] []) <| Maybe.map
                             (\lvl -> (case lvl of
                                           Suburb -> "Suburb:"
                                           Town -> "Town:"
                                           Council -> "Council:"
                                           City -> "City:"
                                           State -> "State:"
                                      )
                             |> \t -> div [class "box"]
                                  [ p [class "title"] [text t]
                                  , div [class "tags"] (List.map viewLabel <| getUniques <| List.map (levelFunction lvl) model.targets)
                                  , viewStats model.boundaries model.targets
                                  ]
                             ) model.level
                       ]
                 ]

getUniques xs = List.foldl (\x -> \acc -> if List.member x acc then acc else x::acc) [] xs

levelFunction lvl = case lvl of
                        Suburb -> .suburb
                        Town -> .town
                        Council -> .council
                        City -> .city
                        State -> .state

levelFilter : Level -> List String -> Properties -> Bool
levelFilter lvl locs prop = let lkp = levelFunction lvl
                     in List.member (lkp prop) locs

viewStats : Boundaries -> List Properties -> Html Msg
viewStats bounds props = let (population, area, ageSum) = List.foldl (\prop -> \(p, a, y) -> (p + prop.population, a + prop.area, y + (toFloat prop.population)*prop.age)) (0,0,0) props
                             age = ageSum/toFloat population
                  in div []
                      [ p [] [strong [] [text "Population: "], text <| floatAsString <| toFloat population, text " people."]
                      , p [] [strong [] [text "Area: "], text <| floatAsString area, text " km²."]
                      , p [] [strong [] [text "Population density: "], text <| floatAsString (toFloat population / area), text " people per km²."]
                      , let popDense = (toFloat population / area)/(bounds.popDenseBounds |> \(_,ma) -> ma) |> \unit -> (1+unit)*(1+unit)*(1+unit)*(1+unit)*(1+unit) - 1
                            classCol = if popDense > 0.75
                                       then "is-danger"
                                       else if popDense > 0.25
                                            then "is-warning"
                                            else "is-success"
                        in progress [class <| "progress "++classCol, value <| String.fromFloat popDense] []
                      , p [] [strong [] [text "Median age: "], text <| floatAsString age, text " years."]
                      ]

floatAsString : Float -> String
floatAsString f = let round2 v = (toFloat <| truncate <| 100 * v)/100
                  in if f > 1000000
                  then f/1000000 |> \v -> String.fromFloat (round2 v) ++ " million"
                  else if f > 1000
                       then f/1000 |> \v -> String.fromFloat (round2 v) ++ " thousand"
                       else String.fromFloat <| round2 f

viewLabel : String -> Html Msg
viewLabel s = span [class "tag is-primary is-large"]
              [text s
              , button [class "delete", onClick <| DropLocation s] []
              ]
