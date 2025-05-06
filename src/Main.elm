module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Random



-- | MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- | MODEL


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Photo =
    { url : String }


type ThumbnailSize
    = Small
    | Medium
    | Large


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }



-- | VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
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
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error String)


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list"
        , expect = Http.expectString GotPhotos
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded [] _ ->
                    ( model, Cmd.none )

                Loaded photos actual ->
                    randomPhoto (Photo actual) photos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        GotPhotos (Ok responseStr) ->
            case
                String.split "," <| String.dropRight 1 responseStr
            of
                (firstUrl :: _) as urls ->
                    let
                        photos =
                            List.map (\url -> { url = url }) urls
                    in
                    ( { model | status = Loaded photos firstUrl }, Random.generate GotRandomPhoto (Random.uniform (Photo firstUrl) photos) )

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )



-- | HELPER FUNCTIONS


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored _ ->
            status


randomPhoto : Photo -> List Photo -> Random.Generator Photo
randomPhoto actual list =
    let
        filteredList =
            List.filter (\photo -> photo.url /= actual.url) list
    in
    case filteredList of
        first :: rest ->
            Random.uniform first rest

        [] ->
            Random.constant actual


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl photo =
    img
        [ src ("http://elm-in-action.com/" ++ photo.url)
        , classList [ ( "selected", selectedUrl == photo.url ) ]
        , onClick (ClickedPhoto photo.url)
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
