(* Server for the network implementation **************************************)

open Arg
open Format
open Sys
open Unix

module String_set = Set.Make(String)

let config = "network.config"
let init = ref false


let service client_sock =
    dup2 client_sock stdin;
    close client_sock;
    let in_channel = in_channel_of_descr stdin in
    let v = ((Marshal.from_channel in_channel) : (unit -> unit)) in
    v ()

let init_server_sock addr =
    let sock = socket PF_INET SOCK_STREAM 0 in
    try
        bind sock addr;
        listen sock 10;
        sock
    with
    | exn ->
        close sock;
        raise exn

let run_server server_sock client_sock =
    let run () =
        match fork () with
        | 0 ->
            close server_sock;
            service client_sock;
            exit 0
        | _ ->
            ()
    in
    let z =
        try run ()
        with
        | exn ->
            close client_sock;
            raise exn
    in
    close client_sock;
    z

let shutdown_server _ =
    exit 0

let launch_server addr =
    set_signal sighup Signal_ignore;
    let server_sock = init_server_sock addr in
    while true do
        let (client_sock, _) = accept server_sock in
        run_server server_sock client_sock
    done

let establish_server () =
    set_signal sigint (Signal_handle shutdown_server);
    let host_name = gethostname () in
    let host_entry = gethostbyname host_name in
    let host_addr = host_entry.h_addr_list.(0) in
    let addr = ADDR_INET (host_addr, 1400) in
    launch_server addr


let () = establish_server ()
