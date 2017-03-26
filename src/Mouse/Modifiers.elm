module Mouse.Modifiers
    exposing
        ( Modifiers
        , modifiers
        , hasAlt
        , hasCtrl
        , hasMeta
        , hasShift
        )

{-| This module deals with modifier keys that may be pressed during mouse events.

# Obtaining the Modifiers

This module is intended to extract a set of modifier keys from a `MouseEvent`, as defined in the
[DOM Level 2](https://www.w3.org/TR/DOM-Level-2-Events/events.html#Events-MouseEvent)
specification.

@docs Modifiers, modifiers

# Testing the Modifier Keys

@docs hasAlt, hasCtrl, hasMeta, hasShift

-}

import Json.Decode as Json


{-| Type representing the modifiers keys.
-}
type Modifier
    = AltKey
    | CtrlKey
    | MetaKey
    | ShiftKey


{-| Type representing a set of modifier keys.
-}
type Modifiers
    = Modifiers (List Modifier)


fromList : List Modifier -> Modifiers
fromList =
    Modifiers


toList : Modifiers -> List Modifier
toList (Modifiers list) =
    list


{-| Checks if the Alt key is present in the given set.
-}
hasAlt : Modifiers -> Bool
hasAlt =
    toList >> List.member AltKey


{-| Checks if the Ctrl key is present in the given set.
-}
hasCtrl : Modifiers -> Bool
hasCtrl =
    toList >> List.member CtrlKey


{-| Checks if the Meta key is present in the given set.
-}
hasMeta : Modifiers -> Bool
hasMeta =
    toList >> List.member MetaKey


{-| Checks if the Shift key is present in the given set.
-}
hasShift : Modifiers -> Bool
hasShift =
    toList >> List.member ShiftKey


{-| JSON parser for obtaining a set of modifiers from a MouseEvent.
-}
modifiers : Json.Decoder Modifiers
modifiers =
    let
        assemble hasAlt hasCtrl hasMeta hasShift =
            Modifiers <|
                (addWhen hasAlt AltKey)
                    ++ (addWhen hasCtrl CtrlKey)
                    ++ (addWhen hasMeta MetaKey)
                    ++ (addWhen hasShift ShiftKey)

        addWhen bool val =
            if bool then
                [ val ]
            else
                []
    in
        Json.map4 assemble
            (Json.field "altKey" Json.bool)
            (Json.field "ctrlKey" Json.bool)
            (Json.field "metaKey" Json.bool)
            (Json.field "shiftKey" Json.bool)
