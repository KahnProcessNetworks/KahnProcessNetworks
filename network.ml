(* Implementation with processes communicating through the network. ***********)

open Format
open Marshal
open Miscellaneous
open Sys
open Unix

module String_set = Set.Make(String)

let config = "network.config"


(* Auxilliar functions ********************************************************)

let get_addr () =
    trace "get_addr";
    let host_name = gethostname () in
    let host_entry = gethostbyname host_name in
    host_entry.h_addr_list.(0)

let make_sockaddr port =
    trace "make_sockaddr";
    let addr = get_addr () in
    ADDR_INET (addr, port)

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


(* Client *********************************************************************)

let shutdown_client _ =
    exit 0

let run_client () =
    trace "run_client";
    set_signal sigint (Signal_handle shutdown_client);
    let port = 1400 in
    let (host_name, host_addr) = choose_computer () in
    let addr = ADDR_INET (host_addr, port) in
    let sock = socket PF_INET SOCK_STREAM 0 in
    connect sock addr;
    dup2 sock stdout;
    close sock;
    let file_descr = openfile "a.out" [O_RDONLY] 0 in
    retransmit file_descr stdout; 
(*
    let out_channel = out_channel_of_descr stdout in
    let e = (fun () -> Format.printf "Salut@.") in
    to_channel out_channel (e : unit -> unit) [ Closures ];
*)
    shutdown_client ()


(* Module *********************************************************************)

type 'a process = unit -> 'a
type 'a port =
{
    addr: sockaddr;
    mutable channel: 'a;
    mutable is_not_active: bool
}
type 'a in_port = in_channel port
type 'a out_port = out_channel port

let new_port addr channel is_not_active =
    trace "new_port";
    {
        addr = addr;
        channel = channel;
        is_not_active = is_not_active;
    }

let new_channel =
    let port = ref 1399 in
    fun () ->
        trace "new_channel";
        incr port;
        let sockaddr = make_sockaddr !port in
        let in_channel = in_channel_of_descr stdin in
        let in_port = new_port sockaddr in_channel true in
        let out_channel = out_channel_of_descr stdout in
        let out_port = new_port sockaddr out_channel true in
        (in_port, out_port)

let new_socket addr =
    let sockdomain = domain_of_sockaddr addr in
    let socktype = SOCK_STREAM in
    socket sockdomain socktype 0

let rec rec_connect sock addr =
    try
        connect sock addr
    with
        _ -> rec_connect sock addr

let retransmit in_channel out_channel =
    while true do
        let v = ((Marshal.from_channel in_channel) : 'a) in
        Marshal.to_channel out_channel (v : 'a) [ Marshal.Closures ]
    done

let put (v : 'a) p () =
    trace "put";
    if (p.is_not_active)
    then
        begin
        let (in_file_descr, out_file_descr) = pipe () in
        match Unix.fork () with
        | 0 ->
            let in_channel = in_channel_of_descr in_file_descr in
            close out_file_descr;
            let sock = new_socket p.addr in
            rec_connect sock p.addr;
            let out_channel = out_channel_of_descr sock in
            retransmit in_channel out_channel
        | _ ->
            close in_file_descr;
            let out_channel = out_channel_of_descr out_file_descr in
            p.channel <- out_channel;
            p.is_not_active <- false;
        end;
    Marshal.to_channel p.channel (v : 'a) [ Marshal.Closures ]

let rec get p ()=
    trace "get ";
    if (p.is_not_active)
    then
        begin
        let (in_file_descr, out_file_descr) = pipe () in
        match Unix.fork () with
        | 0 ->
            close in_file_descr;
            let out_channel = out_channel_of_descr out_file_descr in
            let sock = new_socket p.addr in
            bind sock p.addr;
            listen sock 10;
            let (file_descr, _) = accept sock in
            let in_channel = in_channel_of_descr file_descr in
            retransmit in_channel out_channel
        | _ ->
            let in_channel = in_channel_of_descr in_file_descr in
            close out_file_descr;
            p.channel <- in_channel;
            p.is_not_active <- false;
        end;
    ((Marshal.from_channel p.channel) : 'a)

let rec doco l () =
    trace "doco";
    match l with
    | [] -> ()
    | hd :: tl ->
        match fork () with
        | 0 ->
            trace "fork (child)";
            hd ()
        | pid ->
            trace "fork (father)";
            doco tl ();
            ignore (waitpid [] pid)

let return v =
    trace "return";
    fun () -> v

let bind e e' () =
    trace "bind";
    let v = e () in
    e' v ()

let run e =
    trace "run";
    (* e () *)
    if argv.(1) = "-init"
    then printf "Cool@."
    else printf "Domage@.";
    handle_unix_error run_client ()
