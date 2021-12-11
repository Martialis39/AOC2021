module Lanternfish exposing (answer, main, realInput)

import Dict exposing (..)
import Html
import List
import Maybe


sampleInput =
    """
    3,4,3,1,2
"""


realInput =
    """
    2,5,3,4,4,5,3,2,3,3,2,2,4,2,5,4,1,1,4,4,5,1,2,1,5,2,1,5,1,1,1,2,4,3,3,1,4,2,3,4,5,1,2,5,1,2,2,5,2,4,4,1,4,5,4,2,1,5,5,3,2,1,3,2,1,4,2,5,5,5,2,3,3,5,1,1,5,3,4,2,1,4,4,5,4,5,3,1,4,5,1,5,3,5,4,4,4,1,4,2,2,2,5,4,3,1,4,4,3,4,2,1,1,5,3,3,2,5,3,1,2,2,4,1,4,1,5,1,1,2,5,2,2,5,2,4,4,3,4,1,3,3,5,4,5,4,5,5,5,5,5,4,4,5,3,4,3,3,1,1,5,2,4,5,5,1,5,2,4,5,4,2,4,4,4,2,2,2,2,2,3,5,3,1,1,2,1,1,5,1,4,3,4,2,5,3,4,4,3,5,5,5,4,1,3,4,4,2,2,1,4,1,2,1,2,1,5,5,3,4,1,3,2,1,4,5,1,5,5,1,2,3,4,2,1,4,1,4,2,3,3,2,4,1,4,1,4,4,1,5,3,1,5,2,1,1,2,3,3,2,4,1,2,1,5,1,1,2,1,2,1,2,4,5,3,5,5,1,3,4,1,1,3,3,2,2,4,3,1,1,2,4,1,1,1,5,4,2,4,3
    """


type alias Age =
    Int


type alias Count =
    Int


type alias FishDict =
    Dict Age Count


type Action
    = GetOlder Age Count
    | BirthNewFish Count


toAction ( age, count ) =
    case age of
        0 ->
            BirthNewFish count

        v ->
            GetOlder (v - 1) count


parse : String -> List Age
parse str =
    case List.head <| String.words str of
        Just s ->
            String.split "," s
                |> List.filterMap String.toInt

        Nothing ->
            []


answer input =
    parse input
        |> countFish
        |> simulate 256
        |> Dict.toList
        |> List.map Tuple.second
        |> List.sum


gatherFishInDictionary : Age -> FishDict -> FishDict
gatherFishInDictionary fish fishDict =
    Dict.update fish
        (\currentFish ->
            case currentFish of
                Just oldValue ->
                    Just (oldValue + 1)

                Nothing ->
                    Just 1
        )
        fishDict


countFish listOfFish =
    List.foldl gatherFishInDictionary Dict.empty listOfFish


increment : Count -> Maybe Count -> Maybe Count
increment count value =
    case Debug.log "va " value of
        Just oldValue ->
            Just (oldValue + Debug.log "count " count)

        Nothing ->
            Just count


simulate : Int -> FishDict -> FishDict
simulate days fishDict =
    if days == 0 then
        fishDict

    else
        let
            nextDict =
                Dict.toList fishDict
                    |> List.map toAction
                    |> List.foldl
                        (\action dict ->
                            case action of
                                BirthNewFish c ->
                                    dict
                                        |> Dict.update 6 (increment c)
                                        |> Dict.update 8 (increment c)

                                GetOlder age count ->
                                    dict
                                        |> Dict.update age (increment count)
                        )
                        Dict.empty
        in
        simulate (days - 1) (Debug.log "NExt " nextDict)


main : Html.Html a
main =
    Html.text <|
        Debug.toString (answer realInput)



--answer input =
--parse input
--|> loop 0 18
--|> List.length
