module Main exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random



-- | MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, selectedUrl )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- | MODEL


type alias Model =
    { photos : List Thumbnail
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }


type alias Thumbnail =
    { url : String }


type ThumbnailSize
    = Small
    | Medium
    | Large


photoArray : Array.Array Thumbnail
photoArray =
    Array.fromList
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]


initialModel : Model
initialModel =
    { photos = Array.toList photoArray
    , selectedUrl = ""
    , chosenSize = Medium
    }



-- | VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ class "surprise-me" ] [ button [ onClick ClickedSurpriseMe ] [ text "Surprise Me!" ] ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ] (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ] (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img [ class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl) ] []
        ]


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label []
        [ input [ type_ "radio", name "size", checked (chosenSize == size), onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]



-- | UPDATE


type Msg
    = ClickedPhoto String
    | GotSelectedIndex Int
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedUrl = url }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )



-- | HELPER FUNCTIONS


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


selectedUrl : Cmd Msg
selectedUrl =
    Random.generate GotSelectedIndex randomPhotoPicker


viewThumbnail : String -> Thumbnail -> Html Msg
viewThumbnail selectedThumbnailUrl thumbnail =
    img
        [ src ("http://elm-in-action.com/" ++ thumbnail.url)
        , classList [ ( "selected", selectedThumbnailUrl == thumbnail.url ) ]
        , onClick (ClickedPhoto thumbnail.url)
        ]
        []


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)
