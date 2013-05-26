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


(* Auxilliar functions ********************************************************)

let retransmit in_file_descr out_file_descr : 'a =
    let in_chan = in_channel_of_descr in_file_descr in
    let out_chan = out_channel_of_descr out_file_descr in
    while (true) do
        let v = (from_channel in_chan : 'a) in
        to_channel out_chan (v : 'a) [Closures];
        flush out_chan
    done;
    (from_channel in_chan : 'a)

let find_computers () =
    let computers = ref [] in
    let in_chan = open_in network_config in
    try
        while true do
            let computer = input_line in_chan in
            computers := computer :: !computers
        done;
        !computers
    with
    | End_of_file ->
        close_in in_chan;
        !computers

let choose_computers n =
    let computers = find_computers () in
    let rec choose n computers =
        match n with
        | 0 -> []
        | _ ->
            match computers with
            | [] -> failwith "empty network 1"
            | _ ->
                let p _ = Random.bool () in
                let (l1, l2) = List.partition p computers in
                let computers = l1 @ l2 in
                let l3 = choose (n - 1) (List.tl computers) in
                match computers with
                | [] -> failwith "empty network 2"
                | computer :: tl ->
                    try
                        (computer, inet_addr_of_string computer) :: l3
                    with
                    | _ ->
                        try
                            let host_entry = gethostbyname computer in
                            (computer, host_entry.h_addr_list.(0)) :: l3
                        with
                        | _ -> choose n tl
    in
    choose n computers


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
        ((!id, In, !wait_level, !current_computer), (!id, Out, !wait_level, !current_computer))

let rec rec_connect sock addr =
    try connect sock addr
    with _ -> rec_connect sock addr

let put (v : 'a) p () =
    try
        let chan = Hashtbl.find out_chan_tbl p in
        to_channel chan (v : 'a) [Closures];
        flush chan
    with
    | Not_found ->
        printf "First put@.";
        let (in_file_descr, out_file_descr) = pipe () in
        match fork () with
        | 0 ->
            (* Establish a relay *)
            printf "Put relay@.";
            close out_file_descr;
            printf "Put gethostbyname \"%s\"@." !father_computer;
            let host = gethostbyname !father_computer in
            let addr = ADDR_INET (host.h_addr_list.(0), 1401) in
            let sock = socket PF_INET SOCK_STREAM 0 in
            printf "Put connect@.";
            rec_connect sock addr;
            printf "Put send information to master@."; 
            let out_chan = out_channel_of_descr sock in
            let in_chan = in_channel_of_descr sock in
            to_channel out_chan (p : port) [Closures];
            flush out_chan;
            printf "Put receive information from master@.";
            let host_addr = (from_channel in_chan : inet_addr) in
            close sock;
            let addr = ADDR_INET (host_addr, 1402) in
            printf "Put connect to real process@."; 
            let sock = socket PF_INET SOCK_STREAM 0 in
            rec_connect sock addr;
            printf "Receive information from master@.";
            printf "Before Put retransmission@.";
            retransmit in_file_descr sock
        | pid ->
            (* Continue with put to relay *)
            printf "Father of put relay@.";
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
        printf "First get@.";
        let (in_file_descr, out_file_descr) = pipe () in
        match fork () with
        | 0 ->
            printf "Get relay@.";
            (* Establish a relay *)
            close in_file_descr;
            printf "Get gethostbyname \"%s\"@." !father_computer;
            let host = gethostbyname !father_computer in
            let addr = ADDR_INET (host.h_addr_list.(0), 1401) in
            printf "Get socket@.";
            let sock = socket PF_INET SOCK_STREAM 0 in
            printf "Get connect@.";
            rec_connect sock addr;
            printf "Get send information to master@."; 
            let out_chan = out_channel_of_descr sock in
            let in_chan = in_channel_of_descr sock in
            printf "Get before Marshal to master@.";
            to_channel out_chan (p : port) [Closures];
            flush out_chan;
            printf "Get befoore Marshal from master@.";
            let _ = (from_channel in_chan : inet_addr) in
            close sock;
            printf "Get lose in out@.";
            printf "Get receive information from master@.";
            let host_name = gethostname () in
            let host_entry = gethostbyname host_name in
            let host_addr = host_entry.h_addr_list.(0) in
            let addr = ADDR_INET (host_addr, 1402) in
            let server_sock = new_socket addr in
            let (client_sock, _) = accept server_sock in
            printf "Before Get retransmission@.";
            (retransmit client_sock out_file_descr : 'a)
        | pid ->
            (* Continue with put to relay *)
            printf "Father of get relay@.";
            mem_for_killall pid;
            close out_file_descr;
            let chan = in_channel_of_descr in_file_descr in
            Hashtbl.add in_chan_tbl p chan;
            (from_channel chan : 'a)            

let inet_addr_of_sockaddr sock =
    match sock with
    | ADDR_INET (inet_addr, _) -> inet_addr
    | _ -> assert false

let oppose =
    function
    | In -> Out
    | Out -> In

let distribute (l : unit process list) : unit =
    (* Select computers over the network *)
    let n = List.length l in
    let computers = choose_computers n in
    let l = List.combine l computers in
    match fork () with
    | 0 ->
        (* Launch the service information about channels *)
        let mem = Hashtbl.create 50 in
        let rec service server_sock =
            printf "Service communication@.";
            let (client_sock, client_addr1) = accept server_sock in
            printf "Connexion admise au service de communication@.";
            let in_chan = in_channel_of_descr client_sock in
            let out_chan1 = out_channel_of_descr client_sock in
            printf "Avant Marshal service de communication@.";
            let ((id, key, wait_level, father) as p) = (from_channel in_chan : port) in
            printf "Après Marshal service de communication id=%d wait_level=%d father=%s@." id wait_level father;
            try
                printf "Echange de données au service de communication@.";
                let (client_addr2, out_chan2) = Hashtbl.find mem (id, oppose key, wait_level, father) in
                to_channel out_chan1 (inet_addr_of_sockaddr client_addr2) [Marshal.Closures];
                flush out_chan1;
                to_channel out_chan2 (inet_addr_of_sockaddr client_addr1) [Marshal.Closures];
                flush out_chan2;
                close_out out_chan1;
                close_out out_chan2
            with
            | Not_found ->
                printf "Service de com ajout Hashtbl@.";
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
                    printf "Before Marshal \"%s\"@." !current_computer;
                    to_channel out_chan (m : 'a request) [Closures];
                    printf "After Marshal@.";
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
        printf "Wait informations demandées@.";
        let in_chan = in_channel_of_descr client_sock in
        let m = ((from_channel in_chan) : 'a request) in
        let (father, computer, e') = m in
        printf "Wait informations recues de \"%s\"@." father;
        close client_sock;
        father_computer := father;
        current_computer := computer;
        printf "Wait run processus father=\"%s\" current=\"%s\"" father computer;
        (e' () : 'a)
    in
    incr wait_level;
    printf "Wait wait_level=%d@." !wait_level;
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
                printf "Run processus@.";
                (e () : 'a);
                kill pid sigkill;
                exit 0
    in
    handle_unix_error run ()
