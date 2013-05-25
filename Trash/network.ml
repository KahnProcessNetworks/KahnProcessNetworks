(* Server for the network implementation **************************************)

open Arg
open Format
open String
open Sys
open Unix

module String_set = Set.Make(String)

let config = "network.config"
let init = ref false


(* Auxilliar functions ********************************************************)

let find_computers () =
    let computers = ref String_set.empty in
    let in_channel = open_in config in
    try
        while true do
            let computer = input_line in_channel in
            computers := String_set.add computer !computers
        done;
        !computers
    with
    | End_of_file -> !computers

let choose_computer () =
    let computers = find_computers () in
    let rec choose computers =
        let computer = String_set.choose computers in
        try
            (computer, inet_addr_of_string computer)
        with
        | Failure _ ->
            try
                let host_entry = gethostbyname computer in
                (computer, host_entry.h_addr_list.(0))
            with
            | _ -> choose (String_set.remove computer computers)
    in
    choose computers

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
    close client_sock;
    let in_channel = in_channel_of_descr stdin in
    let e = ((Marshal.from_channel in_channel) : (unit -> unit)) in
    e ()

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
    try_finalize run () close client_sock

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


(* Client *********************************************************************)

let shutdown_client _ =
    exit 0

let run_client () =
    set_signal sigint (Signal_handle shutdown_client);
    let port = 1400 in
    let (host_name, host_addr) = choose_computer () in
    let addr = ADDR_INET (host_addr, port) in
    let sock = socket PF_INET SOCK_STREAM 0 in
    connect sock addr;
    dup2 sock stdout;
    close sock;
    let out_channel = out_channel_of_descr stdout in
    let e () =
        for i = 1 to 100 do
            printf "%d " i
        done;
        printf "@."
    in
    Marshal.to_channel out_channel (e : unit -> unit) [ Marshal.Closures ];
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
