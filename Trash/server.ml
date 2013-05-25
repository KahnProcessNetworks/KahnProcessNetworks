(* Server for the network implementation **************************************)

open Arg


let server_mode = ref false
let status_mode = ref false

let main () =
    Sys.set_signal Sys.sighup Sys.Signal_ignore;
    let specification_list =
    [
        ("-server", Set_bool server_mode, "set the length of the game area");
        ("-status", Set_bool status_mode, "set the width of the game area");
    ]
    in
    let usage_message = "Usage: " ^ Sys.argv.(0) ^ " [options] \nOptions:" in
    let anonymous_function _ =
        Arg.usage specification_list usage_message;
        exit 1
    in
    Arg.parse specification_list anonymous_function usage_message;
    if (server_mode)
    then Format.printf "server_mode@."
    else
        if (status_mode)
        then Format.printf "server_mode@."
        else Format.printf "no_mode@."
