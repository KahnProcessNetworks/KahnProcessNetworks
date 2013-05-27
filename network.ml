(* Implementation with processes communicating through the network. ***********)

open Format
open Marshal
open Miscellaneous
open Server
open Sys
open Unix

let network_config = "network.config"
let host_config = "host.config"
let process_port = 1400
let communication_port = 1401
let father_computer = ref ""
let current_computer = ref ""


(* Module *********************************************************************)

type 'a process = unit -> 'a
type key = In | Out
type port = int * key * int * string
type 'a in_port = port
type 'a out_port = port
type 'a request = string * string * 'a process

let (in_chan_tbl : (port, in_channel) Hashtbl.t) = Hashtbl.create 17
let (out_chan_tbl : (port, out_channel) Hashtbl.t) = Hashtbl.create 17
let wait_level = ref 0

let new_channel =
    let id = ref 0 in
    fun () ->
        incr id;
        let in_port = (!id, In, !wait_level, !current_computer) in
        let out_port = (!id, Out, !wait_level, !current_computer) in
        (in_port, out_port)

let put (v : 'a) p () =
    try
        let chan = Hashtbl.find out_chan_tbl p in
        to_channel chan (v : 'a) [Closures];
        flush chan
    with
    | Not_found ->
        let (in_file_descr, out_file_descr) = pipe () in
        match fork () with
        | 0 ->
            (* Establish a relay *)
            close out_file_descr;
            let host = gethostbyname !father_computer in
            let addr = ADDR_INET (host.h_addr_list.(0), 1401) in
            let sock = socket PF_INET SOCK_STREAM 0 in
            rec_connect sock addr;
            let out_chan = out_channel_of_descr sock in
            let in_chan = in_channel_of_descr sock in
            to_channel out_chan (p : port) [Closures];
            flush out_chan;
            let host_addr = (from_channel in_chan : inet_addr) in
            close sock;
            let addr = ADDR_INET (host_addr, 1402) in
            let sock = socket PF_INET SOCK_STREAM 0 in
            rec_connect sock addr;
            retransmit in_file_descr sock
        | pid ->
            (* Continue with put to relay *)
            mem_for_killall pid;
            close in_file_descr;
            let chan = out_channel_of_descr out_file_descr in
            Hashtbl.add out_chan_tbl p chan;
            to_channel chan (v : 'a) [Closures];
            flush chan

let get p () =
    try
        let in_chan = Hashtbl.find in_chan_tbl p in
        (from_channel in_chan : 'a)
    with
    | Not_found ->
        let (in_file_descr, out_file_descr) = pipe () in
        match fork () with
        | 0 ->
            (* Establish a relay *)
            close in_file_descr;
            let host = gethostbyname !father_computer in
            let addr = ADDR_INET (host.h_addr_list.(0), 1401) in
            let sock = socket PF_INET SOCK_STREAM 0 in
            rec_connect sock addr;
            let out_chan = out_channel_of_descr sock in
            let in_chan = in_channel_of_descr sock in
            to_channel out_chan (p : port) [Closures];
            flush out_chan;
            let _ = (from_channel in_chan : inet_addr) in
            close sock;
            let host_name = gethostname () in
            let host_entry = gethostbyname host_name in
            let host_addr = host_entry.h_addr_list.(0) in
            let addr = ADDR_INET (host_addr, 1402) in
            let server_sock = new_socket addr in
            let (client_sock, _) = accept server_sock in
            (retransmit client_sock out_file_descr : 'a)
        | pid ->
            (* Continue with put to relay *)
            mem_for_killall pid;
            close out_file_descr;
            let chan = in_channel_of_descr in_file_descr in
            Hashtbl.add in_chan_tbl p chan;
            (from_channel chan : 'a)            

let oppose =
    function
    | In -> Out
    | Out -> In

let distribute (l : unit process list) : unit =
    (* Select computers over the network *)
    let n = List.length l in
    let computers = choose_computers network_config n in
    let l = List.combine l computers in
    match fork () with
    | 0 ->
        (* Launch the service information about channels *)
        let mem = Hashtbl.create 50 in
        let rec service server_sock =
            let (client_sock, client_addr1) = accept server_sock in
            let in_chan = in_channel_of_descr client_sock in
            let out_chan1 = out_channel_of_descr client_sock in
            let ((id, key, wait_level, father) as p) = (from_channel in_chan : port) in
            try
                let (client_addr2, out_chan2) = Hashtbl.find mem (id, oppose key, wait_level, father) in
                to_channel out_chan1 (inet_addr_of_sockaddr client_addr2) [Marshal.Closures];
                flush out_chan1;
                to_channel out_chan2 (inet_addr_of_sockaddr client_addr1) [Marshal.Closures];
                flush out_chan2;
                close_out out_chan1;
                close_out out_chan2
            with
            | Not_found ->
                Hashtbl.add mem p (client_addr1, out_chan1);
                service server_sock
        in
        Server.launch Sequential communication_port service
    | pid ->
        (* Request runs over the network *)
        let rec send l =
            match l with
            | [] -> ()
            | (e, (computer, addr_inet)) :: tl ->
                match fork () with
                | 0 ->
                    (* Send the name of the computer and the process to run. *)
                    let addr = ADDR_INET (addr_inet, 1400) in
                    let sock = socket PF_INET SOCK_STREAM 0 in
                    connect sock addr;
                    let out_chan = out_channel_of_descr sock in
                    let m = (!current_computer, computer, e) in
                    to_channel out_chan (m : 'a request) [Closures];
                    flush out_chan;
                    (* Wait for acknowledgment. *)
                    let in_chan = in_channel_of_descr sock in
                    ignore(from_channel in_chan : unit);
                    close sock;
                    exit 0
                | pid ->
                    mem_for_killall pid;
                    send tl;
                    ignore (waitpid [] pid)
        in
        mem_for_killall pid;
        send l

let rec doco l () =
    match l with
    | [] -> ()
    | _ -> distribute l

let return v =
    fun () -> v

let bind e e' () =
    let v = e () in
    e' v ()

let rec wait () : 'a =
    let service client_sock =
        let in_chan = in_channel_of_descr client_sock in
        let m = ((from_channel in_chan) : 'a request) in
        let (father, computer, e') = m in
        close client_sock;
        father_computer := father;
        current_computer := computer;
        (e' () : 'a)
    in
    incr wait_level;
    (Server.launch Fork process_port service : 'a)

let run e =
    let in_chan = open_in host_config in
    current_computer := input_line in_chan;
    close_in in_chan;
    let n = Array.length argv in
    let run () =
        if ("-wait" = argv.(n - 1))
        then
            (* Wait until a doco over the network requests a run *)
            (wait () : 'a)
        else
            (* Fork to wait for a doco and run the process *)
            match fork () with
            | 0 -> (wait () : 'a)
            | pid ->
                mem_for_killall pid;
                (e () : 'a);
                kill pid sigkill;
                exit 0
    in
    handle_unix_error run ()
