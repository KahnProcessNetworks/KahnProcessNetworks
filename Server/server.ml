(* Server for the network implementation **************************************)

open Arg
open Unix


let server_mode = ref false
let status_mode = ref false


(* Server *********************************************************************)

let make_sockaddr addr port = ADDR_INET (addr, port)

let establish_server server_fun sockaddr =
    let sockdomain = domain_of_sockaddr sockaddr in
    let socktype = SOCK_STREAM in
    let sock = socket sockdomain socktype 0 in
    bind sock sockaddr;
    listen sock 3;
    let (file_descr, sockaddr) = accept sock in
    match fork () with
    | 0 ->
        let in_channel = in_channel_of_descr file_descr in
        let out_channel = out_channel_of_descr file_descr in
        server_fun in_channel out_channel;
        close sock;
        exit 0
    | pid ->
        let _ = waitpid [] pid in
        close sock;
        ()

let get_my_addr () =
    let host_name = gethostname () in
    let host_entry = gethostbyname host_name in
    host_entry.h_addr_list.(0)

let main_server port server_fun =
    let addr = get_my_addr () in
    let sockaddr = make_sockaddr addr port in
    establish_server server_fun sockaddr

let server_fun in_channel out_channel =
    try
        while true do
            let s = input_line in_channel in 
            let r = String.uppercase s in
            Format.printf "%s@." r;
            if (r = "END")
            then raise Exit;
        done
    with
    | _ -> ()

let run_server port =
    Unix.handle_unix_error (main_server port) server_fun


(* Main ***********************************************************************)

let main () =
    Sys.set_signal Sys.sighup Sys.Signal_ignore;
    let specification_list =
        align        
        [
            ("-server", Set server_mode, " Initialize a new server");
            ("-status", Set status_mode, " Display the network status");
        ]
    in
    let usage_message = "Usage: " ^ Sys.argv.(0) ^ " <options> \nOptions:" in
    let anonymous_function _ =
        Arg.usage specification_list usage_message;
        exit 1
    in
    Arg.parse specification_list anonymous_function usage_message;
    if (!status_mode)
    then Format.printf "status_mode@.";
    if (!server_mode)
    then Format.printf "server_mode@."

let () = main ()
