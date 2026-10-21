module Page.Home exposing (Model, Msg(..), init, update, view)

import Api.Finance as Finance
import Api.Util
import Array
import Browser
import Dict
import Html exposing (Html, button, details, div, footer, form, h1, h2, input, label, p, span, summary, text)
import Html.Attributes exposing (attribute, class, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Style


type alias Model =
    { url : String
    , accounts : List Finance.Account
    , selected : Maybe String
    , transactions : List Finance.Transaction
    , loading : Bool
    , transactionsLoaded : Bool
    , message : Maybe String
    , editor : Maybe Editor
    , name : String
    , unit : String
    , amount : String
    }


type Editor
    = AccountEditor
    | TransactionEditor (Maybe Int)


type Msg
    = RefreshAccounts
    | SelectAccount String
    | Received Finance.FinanceMsg
    | CreateAccount
    | CloseAccount
    | AddTransaction
    | EditTransaction Finance.Transaction
    | RemoveTransaction Int
    | UpdateName String
    | UpdateUnit String
    | UpdateAmount String
    | Cancel
    | Save


init : String -> Model
init url =
    { url = url
    , accounts = []
    , selected = Nothing
    , transactions = []
    , loading = False
    , transactionsLoaded = False
    , message = Nothing
    , editor = Nothing
    , name = ""
    , unit = ""
    , amount = "0"
    }


request : Cmd Finance.FinanceMsg -> Model -> ( Model, Cmd Msg )
request cmd model =
    ( { model | loading = True, message = Nothing }, Cmd.map Received cmd )


refresh : Model -> ( Model, Cmd Msg )
refresh model =
    request (Finance.listAccounts model.url) { model | transactionsLoaded = False }


selectedAccount : Model -> Maybe Finance.Account
selectedAccount model =
    List.filter (\account -> Just account.name == model.selected) model.accounts
        |> List.head


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Received response ->
            case response of
                Finance.ListAccounts result ->
                    case result of
                        Err error ->
                            failed error model

                        Ok accounts ->
                            let
                                accountList =
                                    Array.toList accounts

                                selected =
                                    if List.any (\account -> Just account.name == model.selected) accountList then
                                        model.selected

                                    else
                                        List.head accountList |> Maybe.map .name

                                updated =
                                    { model | accounts = accountList, selected = selected, transactions = [], transactionsLoaded = False }
                            in
                            case selected of
                                Just name ->
                                    request (Finance.listTransactions model.url name) updated

                                Nothing ->
                                    ( { updated | loading = False }, Cmd.none )

                Finance.ListTransactions result ->
                    case result of
                        Err error ->
                            failed error model

                        Ok transactions ->
                            ( { model | transactions = Array.toList transactions, transactionsLoaded = True, loading = False }, Cmd.none )

                Finance.OpenAccount result ->
                    case result of
                        Ok _ ->
                            mutationResult result { model | selected = Just (String.trim model.name) }

                        Err error ->
                            failed error model

                Finance.CloseAccount result ->
                    mutationResult result model

                Finance.NewTransaction result ->
                    mutationResult result model

                Finance.EditTransaction result ->
                    mutationResult result model

                Finance.RemoveTransaction result ->
                    mutationResult result model

        _ ->
            if model.loading then
                ( model, Cmd.none )

            else
                updateReady msg model


failed : Http.Error -> Model -> ( Model, Cmd Msg )
failed error model =
    ( { model | loading = False, message = Just (Api.Util.httpErrorToString error) }, Cmd.none )


mutationResult : Result Http.Error String -> Model -> ( Model, Cmd Msg )
mutationResult result model =
    case result of
        Err error ->
            failed error model

        Ok _ ->
            refresh { model | editor = Nothing }


updateReady : Msg -> Model -> ( Model, Cmd Msg )
updateReady msg model =
    case msg of
        RefreshAccounts ->
            refresh model

        SelectAccount name ->
            request (Finance.listTransactions model.url name)
                { model | selected = Just name, transactions = [], transactionsLoaded = False, editor = Nothing }

        CreateAccount ->
            ( { model | editor = Just AccountEditor, name = "", unit = "", amount = "0", message = Nothing }, Cmd.none )

        CloseAccount ->
            case selectedAccount model of
                Just account ->
                    request (Finance.closeAccount model.url account.name) model

                Nothing ->
                    ( model, Cmd.none )

        AddTransaction ->
            ( { model | editor = Just (TransactionEditor Nothing), name = "", amount = "0", message = Nothing }, Cmd.none )

        EditTransaction transaction ->
            ( { model | editor = Just (TransactionEditor (Just transaction.id)), name = transaction.transactionName, amount = String.fromInt transaction.ammount, message = Nothing }, Cmd.none )

        RemoveTransaction id ->
            case model.selected of
                Just account ->
                    request (Finance.removeTransaction model.url account id) model

                Nothing ->
                    ( model, Cmd.none )

        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        UpdateUnit unit ->
            ( { model | unit = unit }, Cmd.none )

        UpdateAmount amount ->
            ( { model | amount = amount }, Cmd.none )

        Cancel ->
            ( { model | editor = Nothing, message = Nothing }, Cmd.none )

        Save ->
            save model

        _ ->
            ( model, Cmd.none )


validName : String -> Bool
validName name =
    String.split ":" name |> List.all (\part -> not (String.isEmpty (String.trim part)))


parseAmount : String -> Maybe Int
parseAmount amount =
    let
        compact =
            String.filter (\character -> not (List.member character [ ',', ' ', '\u{00A0}', '\u{202F}', '\t', '\n', '\u{000D}', '_', '\'', '$', '€', '£', '¥', '₹', '₩' ])) amount
                |> String.replace "−" "-"

        normalized =
            if String.startsWith "(" compact && String.endsWith ")" compact then
                let
                    unsigned =
                        String.dropLeft 1 compact |> String.dropRight 1
                in
                if String.startsWith "-" unsigned || String.startsWith "+" unsigned then
                    ""

                else
                    "-" ++ unsigned

            else
                compact
    in
    case String.split "." normalized of
        [ whole ] ->
            String.toInt whole

        [ whole, fraction ] ->
            if not (String.isEmpty fraction) && String.all ((==) '0') fraction then
                String.toInt whole

            else
                Nothing

        _ ->
            Nothing


save : Model -> ( Model, Cmd Msg )
save model =
    let
        name =
            String.trim model.name

        invalid message =
            ( { model | message = Just message }, Cmd.none )
    in
    if not (validName name) then
        invalid "Enter a name with non-empty folder names separated by colons."

    else
        case parseAmount model.amount of
            Nothing ->
                invalid "Enter a whole-number amount, such as 100,000 or $100,000. Use a minus sign or parentheses for a decrease. Fractional amounts are not supported."

            Just amount ->
                case model.editor of
                    Just AccountEditor ->
                        let
                            unit =
                                if String.isEmpty (String.trim model.unit) then
                                    "USD"

                                else
                                    String.trim model.unit
                        in
                        request (Finance.openAccount model.url name unit amount) model

                    Just (TransactionEditor id) ->
                        case model.selected of
                            Just account ->
                                request
                                    (case id of
                                        Just transactionId ->
                                            Finance.editTransaction model.url account transactionId name amount

                                        Nothing ->
                                            Finance.newTransaction model.url account name amount
                                    )
                                    model

                            Nothing ->
                                invalid "Select an account first."

                    Nothing ->
                        ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Heft"
    , body =
        [ div [ class "space-y-6" ]
            [ div [ class Style.toolbar ]
                [ h1 [ class (Style.heading ++ " mr-auto text-xl sm:text-2xl") ] [ text "Heft" ]
                , p [ class "text-muted" ] [ text "Personal financial tracking" ]
                ]
            , case model.message of
                Just message ->
                    p [ class Style.message, attribute "role" "alert" ] [ text message ]

                Nothing ->
                    text ""
            , div
                [ class "grid min-h-[70svh] grid-cols-1 gap-4 lg:grid-cols-[20rem_minmax(0,1fr)]"
                , attribute "aria-busy"
                    (if model.loading then
                        "true"

                     else
                        "false"
                    )
                ]
                [ viewAccounts model
                , viewTransactions model
                ]
            ]
        ]
    }


action : Model -> String -> Msg -> String -> Html Msg
action model styling msg caption =
    button [ type_ "button", class styling, disabled model.loading, onClick msg ] [ text caption ]


viewAccounts : Model -> Html Msg
viewAccounts model =
    Html.aside [ class (Style.panel ++ " space-y-4 p-4"), attribute "aria-label" "Accounts" ]
        [ h2 [ class Style.heading ] [ text "Accounts" ]
        , div [ class Style.toolbar ]
            [ action model Style.buttonSecondary RefreshAccounts "Refresh accounts"
            , action model Style.buttonPrimary CreateAccount "Create account"
            ]
        , if model.editor == Just AccountEditor then
            viewEditor model AccountEditor

          else
            text ""
        , if List.isEmpty model.accounts then
            p [ class "text-muted" ]
                [ text
                    (if model.loading then
                        "Loading accounts…"

                     else
                        "No accounts yet. Create one to get started."
                    )
                ]

          else
            div [ class "space-y-2" ]
                (viewFolders .name (viewAccount model) model.accounts)
        , case selectedAccount model of
            Just account ->
                div [ class "space-y-2 border-t border-line pt-4" ]
                    [ button [ type_ "button", class Style.buttonQuiet, disabled (model.loading || account.balance /= 0), onClick CloseAccount ] [ text "Close selected account" ]
                    , if account.balance /= 0 then
                        p [ class "text-xs text-muted" ] [ text "An account must have a zero balance before closing." ]

                      else
                        text ""
                    ]

            Nothing ->
                text ""
        ]


viewAccount : Model -> String -> Finance.Account -> Html Msg
viewAccount model name account =
    button
        [ type_ "button"
        , class
            ((if model.selected == Just account.name then
                Style.buttonPrimary

              else
                Style.buttonSecondary
             )
                ++ " flex w-full items-center justify-between gap-3 text-left"
            )
        , disabled model.loading
        , onClick (SelectAccount account.name)
        , attribute "aria-pressed"
            (if model.selected == Just account.name then
                "true"

             else
                "false"
            )
        ]
        [ span [ class "min-w-0 break-words" ] [ text name ]
        , span [ class Style.money ] [ text (money account.unit account.balance) ]
        ]


viewTransactions : Model -> Html Msg
viewTransactions model =
    Html.section [ class (Style.panel ++ " flex min-h-[60svh] flex-col"), attribute "aria-label" "Transactions" ]
        (case selectedAccount model of
            Nothing ->
                [ p [ class "p-4 text-muted" ] [ text "Select an account to view its transactions." ] ]

            Just account ->
                [ div [ class "space-y-4 border-b border-line p-4" ]
                    [ h2 [ class (Style.heading ++ " break-words") ] [ text (String.join " / " (String.split ":" account.name)) ]
                    , div [ class Style.toolbar ]
                        [ action model Style.buttonSecondary RefreshAccounts "Refresh transactions"
                        , action model Style.buttonPrimary AddTransaction "Add transaction"
                        ]
                    , case model.editor of
                        Just (TransactionEditor id) ->
                            viewEditor model (TransactionEditor id)

                        _ ->
                            text ""
                    ]
                , div [ class "flex-1 space-y-3 p-4" ]
                    (if not model.transactionsLoaded then
                        [ p [ class "text-muted", attribute "role" "status" ]
                            [ text
                                (if model.loading then
                                    "Loading transactions…"

                                 else
                                    "Transactions could not be loaded. Refresh to try again."
                                )
                            ]
                        ]

                     else if List.isEmpty model.transactions then
                        [ p [ class "text-muted" ] [ text "No transactions yet." ] ]

                     else
                        viewFolders .transactionName (viewTransaction model account.unit) model.transactions
                    )
                , viewTotals model account
                ]
        )


viewTransaction : Model -> String -> String -> Finance.Transaction -> Html Msg
viewTransaction model unit name transaction =
    div [ class "flex flex-wrap items-center gap-3 border-b border-line py-2" ]
        [ span [ class "min-w-0 flex-1 break-words" ] [ text name ]
        , span [ class Style.money ] [ text (money unit transaction.ammount) ]
        , div [ class Style.toolbar ]
            [ action model Style.buttonQuiet (EditTransaction transaction) "Edit"
            , action model Style.buttonQuiet (RemoveTransaction transaction.id) "Remove"
            ]
        ]


viewTotals : Model -> Finance.Account -> Html Msg
viewTotals model account =
    let
        increase =
            List.map .ammount model.transactions |> List.filter ((<) 0) |> List.sum

        decrease =
            List.map .ammount model.transactions |> List.filter ((>) 0) |> List.sum

        total caption amount =
            div [ class "space-y-1" ]
                [ Html.dt [ class "text-xs text-muted" ] [ text caption ]
                , Html.dd [ class Style.money ]
                    [ text
                        (if model.transactionsLoaded then
                            money account.unit amount

                         else
                            "—"
                        )
                    ]
                ]
    in
    footer [ class "sticky bottom-0 border-t border-line bg-surface p-4", attribute "aria-live" "polite" ]
        [ Html.dl [ class "grid grid-cols-2 gap-4 xl:grid-cols-5" ]
            [ total "Initial balance" (account.balance - increase - decrease)
            , total "Increase" increase
            , total "Decrease" decrease
            , total "Net change" (increase + decrease)
            , total "Current total" account.balance
            ]
        ]


money : String -> Int -> String
money unit amount =
    String.fromInt amount ++ " " ++ unit


viewEditor : Model -> Editor -> Html Msg
viewEditor model editor =
    let
        field caption hint fieldType contents change =
            label [ class Style.label ]
                [ span [] [ text caption ]
                , input [ class Style.input, type_ fieldType, placeholder hint, value contents, onInput change, disabled model.loading ] []
                ]
    in
    form [ class "space-y-3 border border-line p-3", onSubmit Save ]
        [ h2 [ class Style.heading ]
            [ text
                (case editor of
                    AccountEditor ->
                        "Create account"

                    TransactionEditor Nothing ->
                        "Add transaction"

                    TransactionEditor (Just _) ->
                        "Edit transaction"
                )
            ]
        , field "Name / folder path" "expenses:grocery:snacks" "text" model.name UpdateName
        , if editor == AccountEditor then
            field "Unit (defaults to USD)" "USD" "text" model.unit UpdateUnit

          else
            text ""
        , field
            (if editor == AccountEditor then
                "Initial balance"

             else
                "Amount"
            )
            "0"
            "text"
            model.amount
            UpdateAmount
        , p [ class "text-xs text-muted" ] [ text "Use colons for folders. Amounts may include currency symbols and separators, such as $100,000. Use a minus sign or parentheses for a decrease; whole numbers only." ]
        , div [ class Style.toolbar ]
            [ button [ type_ "submit", class Style.buttonPrimary, disabled model.loading ] [ text "Save" ]
            , action model Style.buttonQuiet Cancel "Cancel"
            ]
        ]


viewFolders : (a -> String) -> (String -> a -> Html msg) -> List a -> List (Html msg)
viewFolders getName render items =
    viewFolderLevel render (List.map (\item -> ( String.split ":" (getName item), item )) items)


viewFolderLevel : (String -> a -> Html msg) -> List ( List String, a ) -> List (Html msg)
viewFolderLevel render items =
    let
        leaves =
            List.filterMap
                (\( path, item ) ->
                    case path of
                        [ name ] ->
                            Just (render name item)

                        _ ->
                            Nothing
                )
                items

        folders =
            List.foldr
                (\( path, item ) groups ->
                    case path of
                        name :: next :: rest ->
                            Dict.update name (\children -> Just (( next :: rest, item ) :: Maybe.withDefault [] children)) groups

                        _ ->
                            groups
                )
                Dict.empty
                items
    in
    leaves
        ++ (Dict.toList folders
                |> List.map
                    (\( name, children ) ->
                        details [ attribute "open" "", class "space-y-2" ]
                            [ summary [ class "cursor-pointer break-words py-2 text-muted focus-visible:outline-accent" ] [ text name ]
                            , div [ class Style.folder ] (viewFolderLevel render children)
                            ]
                    )
           )
