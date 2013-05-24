(* Server for the network implementation **************************************)

open Arg
open Sys
open Unix

let network_ip = ref ""
let server_mode = ref false
let client_mode = ref false


(* Auxilliar functions ********************************************************)

let retransmit in_file_descr out_file_descr =
    let buffer_size = 4096 in
    let buffer = String.create buffer_size in
    let rec copy() =
        match read in_file_descr buffer 0 buffer_size with
        | 0 -> ()
        | n ->
            ignore (write out_file_descr buffer 0 n);
            copy ()
    in
    copy ()


let try_finalize f x finally y =
    let z =
        try f x
        with exn -> finally y; raise exn
    in
    finally y;
    z

let rec restart_on_EINTR f x =
    try f x
    with Unix_error (EINTR, _, _) -> restart_on_EINTR f x
  

(* Server *********************************************************************)

(*
    Port 1400: communication for synchronisation
*)

let init_server_sock addr =
    let server_sock_domain = PF_INET in
    let server_sock_type = SOCK_STREAM in
    let server_sock = socket server_sock_domain server_sock_type 0 in
    try
        bind server_sock addr;
        listen server_sock 10;
        server_sock
    with
    | exn ->
        close server_sock;
        raise exn

let run_server service server_sock client_sock =
    let run () =
        match fork () with
        | 0 ->
            close server_sock;
            service client_sock;
            exit 0
        | _ ->
            ()
    in
    try_finalize run () close client_sock

let launch_server addr service =
    ignore (signal sigpipe Signal_ignore);
    let server_sock = init_server_sock addr in
    while true do
        let (client_sock, _) = restart_on_EINTR accept server_sock in
        run_server service server_sock client_sock
    done

let establish_server () =
    let port = 1400 in
    let host_name = gethostname () in
    let host_entry = gethostbyname host_name in
    let host_addr = host_entry.h_addr_list.(0) in
    let addr = ADDR_INET (host_addr, port) in
    let service client_sock =
        dup2 client_sock stdin;
        (* dup2 client_sock stdout; *)
        (* dup2 client_sock stderr; *)
        close client_sock;
        retransmit stdin stdout
    in
    launch_server addr service


(* Client *********************************************************************)

let run_client () =
    let port = 1400 in
    let server_ip = "129.199.129.26" in
    let server_addr = inet_addr_of_string server_ip in
    let addr = ADDR_INET (server_addr, port) in
    let sock = socket PF_INET SOCK_STREAM 0 in
    connect sock addr;
    dup2 sock stdout;
    close sock;
    retransmit stdin stdout;
    close stdout


(* Main ***********************************************************************)

let main () =
    set_signal sighup Signal_ignore;
    let specification_list =
        align        
        [
            ("-client", Set client_mode, " Initialize a new client");
            ("-server", Set server_mode, " Initialize a new server");
        ]
    in
    let usage_message =
        "Usage: " ^ argv.(0) ^ " XXX.XXX.XXX.XXX <options> \nOptions:"
    in
    let anonymous_function s =
        network_ip := s;
        exit 1
    in
    parse specification_list anonymous_function usage_message;
    if (!client_mode)
    then handle_unix_error run_client ();
    if (!server_mode)
    then handle_unix_error establish_server ()

let () = main ()
