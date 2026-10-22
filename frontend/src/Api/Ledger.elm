module Api.Ledger exposing (LedgerMsg(..), exportLedger, importLedger)

import Http
import Json


type LedgerMsg
    = ExportLedger (Result Http.Error Ledger)
    | ImportLedger (Result Http.Error String)


type alias Ledger =
    { ledger : String
    }


ledgerDecoder : Json.Decoder
ledgerDecoder =
    Json.Decode.map1 Ledger (Json.Decode.at [ "ledger" ] Json.Decode.string)


exportLedger : () -> Cmd LedgerMsg
exportLedger _ =
    Http.get { url = Url.Builder.absolute [ "api", "ledger", "export" ] [], expect = Http.expectJson ExportLedger ledgerDecoder }


encodeLedger : String -> Json.Value
encodeLedger ledger =
    Json.Encode.object [ ( "ledger", Json.Encode.string ledger ) ]


importLedger : String -> Cmd LedgerMsg
importLedger ledger =
    Http.request { method = "POST", headers = [], url = Url.Builder.absolute [ "api", "ledger", "import" ] [], body = Http.jsonBody (encodeLedger ledger), expect = Http.expectString ImportLedger, timeout = Nothing, tracker = Nothing }
