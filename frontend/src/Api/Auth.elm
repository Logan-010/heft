module Api.Auth exposing (AuthMsg(..), isLoggedIn, logIn, logOut, register)

import Http
import Json.Encode as Encode
import Url.Builder


type AuthMsg
    = IsLoggedIn (Result Http.Error String)
    | LogIn (Result Http.Error String)
    | LogOut (Result Http.Error String)
    | Register (Result Http.Error String)


isLoggedIn : Cmd AuthMsg
isLoggedIn =
    Http.get
        { url = Url.Builder.absolute [ "api", "auth", "validate" ] []
        , expect = Http.expectString IsLoggedIn
        }


encodeCredentials : String -> String -> Encode.Value
encodeCredentials username password =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "password", Encode.string password )
        ]


logIn : String -> String -> Cmd AuthMsg
logIn username password =
    Http.post
        { url = Url.Builder.absolute [ "api", "auth", "login" ] []
        , body = Http.jsonBody (encodeCredentials username password)
        , expect = Http.expectString LogIn
        }


logOut : Cmd AuthMsg
logOut =
    Http.post
        { url = Url.Builder.absolute [ "api", "auth", "logout" ] []
        , body = Http.emptyBody
        , expect = Http.expectString LogOut
        }


encodeRegister : String -> String -> String -> Encode.Value
encodeRegister username password secret =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "password", Encode.string password )
        , ( "secret", Encode.string secret )
        ]


register : String -> String -> String -> Cmd AuthMsg
register username password secret =
    Http.post
        { url = Url.Builder.absolute [ "api", "auth", "register" ] []
        , body = Http.jsonBody (encodeRegister username password secret)
        , expect = Http.expectString Register
        }
