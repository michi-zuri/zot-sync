module ZotSync exposing (run)

import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions (Script.log "zot-sync: toolchain OK")
