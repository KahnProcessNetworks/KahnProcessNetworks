(* Server for the network implementation **************************************)

open Miscellaneous
open Sys
open Unix


type model =
    | Sequential
    | Fork

let new_socket addr =
    let sock = socket PF_INET SOCK_STREAM 0 in
    try
        bind sock addr;
        listen sock 100;
        sock
    with
    | exn ->
        close sock;
        raise exn

let run model service server_sock : 'a =
    match model with
    | Sequential ->
        service server_sock
    | Fork ->
        let rec progress () : 'a =
            let (client_sock, _) = accept server_sock in
            let fork () =
                match fork () with
                | 0 -> (service client_sock : 'a)
                | pid ->
                    mem_for_killall pid;
                    (progress () : 'a)
            in
            (try_finally fork () close client_sock : 'a)
        in
        (progress () : 'a)

let launch model port service : 'a =
    (* Stop on interactive interrupt signal. *)
    set_signal sigint (Signal_handle killall);
    let host_name = gethostname () in
    let host_entry = gethostbyname host_name in
    let host_addr = host_entry.h_addr_list.(0) in
    let addr = ADDR_INET (host_addr, port) in
    (* Ignore hangup on controlling terminal. *)
    set_signal sighup Signal_ignore;
    let server_sock = new_socket addr in
    (run model service server_sock : 'a)
