module Api.Auth exposing (AuthMsg(..), isLoggedIn, logIn, logOut, register)

import Http
import Json.Encode as Encode
import Url.Builder


type AuthMsg
    = IsLoggedIn (Result Http.Error String)
    | LogIn (Result Http.Error String)
    | LogOut (Result Http.Error String)
    | Register (Result Http.Error String)


isLoggedIn : String -> Cmd AuthMsg
isLoggedIn url =
    Http.get
        { url = Url.Builder.crossOrigin url [ "api", "auth", "validate" ] []
        , expect = Http.expectString IsLoggedIn
        }


encodeCredentials : String -> String -> Encode.Value
encodeCredentials username password =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "password", Encode.string password )
        ]


logIn : String -> String -> String -> Cmd AuthMsg
logIn url username password =
    Http.post
        { url = Url.Builder.crossOrigin url [ "api", "auth", "login" ] []
        , body = Http.jsonBody (encodeCredentials username password)
        , expect = Http.expectString LogIn
        }


logOut : String -> Cmd AuthMsg
logOut url =
    Http.post
        { url = Url.Builder.crossOrigin url [ "api", "auth", "logout" ] []
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


register : String -> String -> String -> String -> Cmd AuthMsg
register url username password secret =
    Http.post
        { url = Url.Builder.crossOrigin url [ "api", "auth", "register" ] []
        , body = Http.jsonBody (encodeRegister username password secret)
        , expect = Http.expectString Register
        }
