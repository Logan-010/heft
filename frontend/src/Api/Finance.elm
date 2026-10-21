module Api.Finance exposing (Account, FinanceMsg(..), Transaction, closeAccount, editTransaction, listAccounts, listTransactions, newTransaction, openAccount, removeTransaction)

import Array exposing (Array)
import Http
import Json.Decode
import Json.Encode
import Url.Builder


type alias Account =
    { name : String
    , unit : String
    , balance : Int
    }


type alias Transaction =
    { id : Int
    , transactionName : String
    , ammount : Int
    , timestamp : Int
    }


type FinanceMsg
    = ListAccounts (Result Http.Error (Array Account))
    | OpenAccount (Result Http.Error String)
    | CloseAccount (Result Http.Error String)
    | ListTransactions (Result Http.Error (Array Transaction))
    | NewTransaction (Result Http.Error String)
    | RemoveTransaction (Result Http.Error String)
    | EditTransaction (Result Http.Error String)


accountsDecoder : Json.Decode.Decoder (Array Account)
accountsDecoder =
    Json.Decode.array
        (Json.Decode.map3
            Account
            (Json.Decode.at [ "name" ] Json.Decode.string)
            (Json.Decode.at [ "unit" ] Json.Decode.string)
            (Json.Decode.at [ "balance" ] Json.Decode.int)
        )


transactionsDecoder : Json.Decode.Decoder (Array Transaction)
transactionsDecoder =
    Json.Decode.array
        (Json.Decode.map4
            Transaction
            (Json.Decode.at [ "id" ] Json.Decode.int)
            (Json.Decode.at [ "transactionName" ] Json.Decode.string)
            (Json.Decode.at [ "ammount" ] Json.Decode.int)
            (Json.Decode.at [ "timestamp" ] Json.Decode.int)
        )


listAccounts : Cmd FinanceMsg
listAccounts =
    Http.get { url = Url.Builder.absolute [ "api", "accounts", "list" ] [], expect = Http.expectJson ListAccounts accountsDecoder }


encodeOpenAccount : String -> String -> Int -> Json.Encode.Value
encodeOpenAccount name unit balance =
    Json.Encode.object [ ( "name", Json.Encode.string name ), ( "unit", Json.Encode.string unit ), ( "balance", Json.Encode.int balance ) ]


openAccount : String -> String -> Int -> Cmd FinanceMsg
openAccount name unit balance =
    Http.post { url = Url.Builder.absolute [ "api", "accounts", "open" ] [], body = Http.jsonBody (encodeOpenAccount name unit balance), expect = Http.expectString OpenAccount }


encodeCloseAccount : String -> Json.Encode.Value
encodeCloseAccount name =
    Json.Encode.object [ ( "name", Json.Encode.string name ) ]


closeAccount : String -> Cmd FinanceMsg
closeAccount name =
    Http.request { method = "DELETE", headers = [], url = Url.Builder.absolute [ "api", "accounts", "close" ] [], body = Http.jsonBody (encodeCloseAccount name), expect = Http.expectString CloseAccount, timeout = Nothing, tracker = Nothing }


encodeListTransactions : String -> Json.Encode.Value
encodeListTransactions name =
    Json.Encode.object [ ( "accountName", Json.Encode.string name ) ]


listTransactions : String -> Cmd FinanceMsg
listTransactions account =
    Http.post { url = Url.Builder.absolute [ "api", "transactions", "list" ] [], body = Http.jsonBody (encodeListTransactions account), expect = Http.expectJson ListTransactions transactionsDecoder }


encodeNewTransaction : String -> String -> Int -> Json.Encode.Value
encodeNewTransaction accountName name ammount =
    Json.Encode.object [ ( "accountName", Json.Encode.string accountName ), ( "name", Json.Encode.string name ), ( "ammount", Json.Encode.int ammount ) ]


newTransaction : String -> String -> Int -> Cmd FinanceMsg
newTransaction account name ammount =
    Http.post { url = Url.Builder.absolute [ "api", "transactions", "new" ] [], body = Http.jsonBody (encodeNewTransaction account name ammount), expect = Http.expectString NewTransaction }


encodeRemoveTransaction : String -> Int -> Json.Encode.Value
encodeRemoveTransaction accountName id =
    Json.Encode.object [ ( "accountName", Json.Encode.string accountName ), ( "id", Json.Encode.int id ) ]


removeTransaction : String -> Int -> Cmd FinanceMsg
removeTransaction accountName id =
    Http.request { method = "DELETE", headers = [], url = Url.Builder.absolute [ "api", "transactions", "delete" ] [], body = Http.jsonBody (encodeRemoveTransaction accountName id), expect = Http.expectString RemoveTransaction, timeout = Nothing, tracker = Nothing }


encodeEditTransaction : String -> Int -> String -> Int -> Json.Encode.Value
encodeEditTransaction accountName id name ammount =
    Json.Encode.object [ ( "accountName", Json.Encode.string accountName ), ( "id", Json.Encode.int id ), ( "name", Json.Encode.string name ), ( "ammount", Json.Encode.int ammount ) ]


editTransaction : String -> Int -> String -> Int -> Cmd FinanceMsg
editTransaction accountName id name ammount =
    Http.request { method = "UPDATE", headers = [], url = Url.Builder.absolute [ "api", "transactions", "edit" ] [], body = Http.jsonBody (encodeEditTransaction accountName id name ammount), expect = Http.expectString EditTransaction, timeout = Nothing, tracker = Nothing }
