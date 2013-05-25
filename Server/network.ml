(* Server for the network implementation **************************************)

open Arg
open Format
open Sys
open Unix

module Ss = Set.Make(String)

let config = "inet_addr.config"
let init = ref false
let addr = ref Ss.empty


(* Auxilliar functions ********************************************************)

let init_addr () =
    let in_channel = open_in config in
    try
        while true do
            let s = input_line in_channel in
            addr := Ss.add s !addr
        done
    with
    | End_of_file -> ()

let rec choose_addr () =
    let s = Ss.choose !addr in
    try inet_addr_of_string s
    with
    | Failure _ ->
        try
            let host_entry = gethostbyname s in
            host_entry.h_addr_list.(0)
        with
        | Not_found ->
            addr := Ss.remove s !addr;
            choose_addr ()
    

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
  

(* Server *********************************************************************)

(*
    Port 1400: communication for synchronisation
*)

let service client_sock =
    dup2 client_sock stdin;
    (* dup2 client_sock stdout; *)
    (* dup2 client_sock stderr; *)
    close client_sock;
    retransmit stdin stdout

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
            printf "Information: server service running@.";
            close server_sock;
            service client_sock;
            printf "Information: server service runned@.";
            exit 0
        | _ ->
            ()
    in
    try_finalize run () close client_sock

let shutdown_server _ =
    printf "\nInformation: server ended@.";
    exit 0

let launch_server addr =
    set_signal sighup Signal_ignore;
    let server_sock = init_server_sock addr in
    while true do
        printf "Information: server accepting connexion@.";
        let (client_sock, client_addr) = accept server_sock in
        let ADDR_INET (client_inet_addr, client_port) = client_addr in
        printf
            "Information: server accepted connexion %s:%d@."
            (string_of_inet_addr client_inet_addr)
            client_port;
        run_server server_sock client_sock
    done

let establish_server () =
    printf "Information: server started@.";
    set_signal sigint (Signal_handle shutdown_server);
    let host_name = gethostname () in
    let host_entry = gethostbyname host_name in
    let host_addr = host_entry.h_addr_list.(0) in
    let addr = ADDR_INET (host_addr, 1400) in
    launch_server addr


(* Client *********************************************************************)

let shutdown_client _ =
    printf "\nInformation: client ended@.";
    exit 0

let run_client () =
    printf "Information: client started@.";
    set_signal sigint (Signal_handle shutdown_client);
    init_addr ();
    let port = 1400 in
    let server_addr = choose_addr () in
    let addr = ADDR_INET (server_addr, port) in
    let sock = socket PF_INET SOCK_STREAM 0 in
    printf
        "Information: client connecting %s:%d@."
        (string_of_inet_addr server_addr)
        port;
    connect sock addr;
    printf "Information: client connected@.";
    printf "Information: client running@.";
    retransmit stdin sock;
    close sock;
    printf "Information: client runned@.";
    shutdown_client ()


(* Main ***********************************************************************)

let main () =
    let specification_list =
        align        
        [
            ("-init", Set init, " Initialize a new machine for the network");
        ]
    in  
    let usage_message =
        "Usage: " ^ argv.(0) ^ " <options>\nOptions:"
    in
    let anonymous_function _ =
        printf "%s" (usage_string specification_list usage_message);
        exit 1
    in
    parse specification_list anonymous_function usage_message;
    if (!init)
    then establish_server ();
    run_client ()

let () = handle_unix_error main ()
