module DOM exposing (Rectangle, boundingClientRect, parentElement, target)

import Json.Decode as Decode exposing (Decoder)


parentElement : Decoder a -> Decoder a
parentElement decoder =
    Decode.field "parentElement" decoder


target : Decoder a -> Decoder a
target decoder =
    Decode.field "target" decoder


boundingClientRect : Decoder Rectangle
boundingClientRect =
    Decode.map3
        (\( x, y ) width height ->
            { top = y
            , left = x
            , width = width
            , height = height
            }
        )
        (position 0 0)
        offsetWidth
        offsetHeight


position : Float -> Float -> Decoder ( Float, Float )
position x y =
    Decode.map5
        (\scrollLeftP scrollTopP offsetLeftP offsetTopP nodeName ->
            case nodeName of
                -- line-charts assumes that the body has no scroll
                -- which holds true for chrome and FF, but not safari, IE and Edge
                -- so, we cheat and say that the body doesn't have scroll!
                "BODY" ->
                    ( x + offsetLeftP, y + offsetTopP )

                _ ->
                    ( x + offsetLeftP - scrollLeftP, y + offsetTopP - scrollTopP )
        )
        scrollLeft
        scrollTop
        offsetLeft
        offsetTop
        (Decode.field "tagName" Decode.string)
        |> Decode.andThen
            (\( x_, y_ ) ->
                offsetParent ( x_, y_ ) (position x_ y_)
            )


offsetParent : a -> Decoder a -> Decoder a
offsetParent x decoder =
    Decode.oneOf
        [ Decode.field "offsetParent" <| Decode.null x
        , Decode.field "offsetParent" decoder
        ]


offsetWidth : Decoder Float
offsetWidth =
    Decode.field "offsetWidth" Decode.float


offsetHeight : Decoder Float
offsetHeight =
    Decode.field "offsetHeight" Decode.float


offsetLeft : Decoder Float
offsetLeft =
    Decode.field "offsetLeft" Decode.float


offsetTop : Decoder Float
offsetTop =
    Decode.field "offsetTop" Decode.float


scrollLeft : Decoder Float
scrollLeft =
    Decode.field "scrollLeft" Decode.float


scrollTop : Decoder Float
scrollTop =
    Decode.field "scrollTop" Decode.float


type alias Rectangle =
    { top : Float
    , left : Float
    , width : Float
    , height : Float
    }
