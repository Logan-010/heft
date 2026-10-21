module Style exposing (buttonPrimary, buttonQuiet, buttonSecondary, folder, heading, input, label, message, money, panel, toolbar)


heading : String
heading =
    "border-l-2 border-accent pl-3 font-medium tracking-tight"


message : String
message =
    "break-words border-l-2 border-accent pl-3 text-muted"


input : String
input =
    "block min-h-11 w-full min-w-0 rounded-none border border-line bg-canvas px-3 py-3 text-base text-ink caret-accent placeholder:text-muted focus:border-accent focus:outline-2 focus:outline-offset-2 focus:outline-accent sm:text-sm"


button : String
button =
    "min-h-11 cursor-pointer rounded-none border px-3 py-2 text-sm focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent disabled:cursor-wait disabled:opacity-40 motion-safe:transition-colors"


buttonPrimary : String
buttonPrimary =
    button ++ " border-accent bg-accent text-on-accent enabled:hover:bg-transparent enabled:hover:text-accent"


buttonSecondary : String
buttonSecondary =
    button ++ " border-line text-ink enabled:hover:border-accent enabled:hover:text-accent"


buttonQuiet : String
buttonQuiet =
    button ++ " border-transparent text-muted enabled:hover:border-line enabled:hover:text-ink"


panel : String
panel =
    "min-w-0 border border-line bg-surface"


toolbar : String
toolbar =
    "flex flex-wrap items-center gap-2"


folder : String
folder =
    "ml-2 space-y-2 border-l border-line pl-3"


money : String
money =
    "whitespace-nowrap tabular-nums"


label : String
label =
    "block min-w-0 space-y-1 text-sm text-muted"
