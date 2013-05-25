(* Server for the network implementation **************************************)

open Arg
open Format
open String
open Sys
open Unix

module String_set = Set.Make(String)

let config = "hosts.config"
let init = ref false
let hosts = ref String_set.empty


(* Auxilliar functions ********************************************************)

let init_hosts () =
    let in_channel = open_in config in
    try
        while true do
            let host_name = input_line in_channel in
            hosts := String_set.add host_name !hosts
        done
    with
    | End_of_file -> ()

let short_of_long host_name =
    try
        let i = index host_name '.' in
        sub host_name 0 i
    with
    | Not_found -> host_name
    

let rec choose_host () =
    let host_name = String_set.choose !hosts in
    try
        if ((short_of_long host_name) = (gethostname ()))
        then raise Exit;
        (host_name, gethostbyname host_name)
    with
    | _ ->
        hosts := String_set.remove host_name !hosts;
        choose_host ()
    

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
        begin
        match client_addr with
        | ADDR_INET (client_inet_addr, client_port) ->
            let host_entry = gethostbyaddr client_inet_addr in
            printf
                "Information: server accepted connexion %s:%d@."
                host_entry.h_name
                client_port
        | _ ->
            failwith "ADDR_UNIX not supported"
        end;
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
    init_hosts ();
    let port = 1400 in
    let (host_name, host_entry) = choose_host () in
    let host_addr = host_entry.h_addr_list.(0) in
    let addr = ADDR_INET (host_addr, port) in
    let sock = socket PF_INET SOCK_STREAM 0 in
    printf
        "Information: client connecting %s:%d@."
        host_name
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
    let usage_meString_setage =
        "Usage: " ^ argv.(0) ^ " <options>\nOptions:"
    in
    let anonymous_function _ =
        printf "%s" (usage_string specification_list usage_meString_setage);
        exit 1
    in
    parse specification_list anonymous_function usage_meString_setage;
    if (!init)
    then establish_server ();
    run_client ()

let () = handle_unix_error main ()
