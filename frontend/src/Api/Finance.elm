module Api.Finance exposing (Account, FinanceMsg(..), listAccounts)

import Array exposing (Array)
import Http
import Json.Decode
import Url.Builder


type FinanceMsg
    = ListAccounts (Result Http.Error (Array Account))


type alias Account =
    { name : String
    , unit : String
    , balance : Int
    }


accountsDecoder : Json.Decode.Decoder (Array Account)
accountsDecoder =
    Json.Decode.array
        (Json.Decode.map3
            Account
            (Json.Decode.at [ "name" ] Json.Decode.string)
            (Json.Decode.at [ "unit" ] Json.Decode.string)
            (Json.Decode.at [ "balance" ] Json.Decode.int)
        )


listAccounts : String -> Cmd FinanceMsg
listAccounts url =
    Http.get { url = Url.Builder.crossOrigin url [ "api", "accounts", "list" ] [], expect = Http.expectJson ListAccounts accountsDecoder }
