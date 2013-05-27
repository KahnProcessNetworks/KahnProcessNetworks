open Format
open Marshal
open Sys
open Unix


(* Constant *******************************************************************)

let trace_enable = false
let pid_list = ref []


(* Trace function *************************************************************)

let trace =
    let i = ref 0 in
    fun s ->
        if (trace_enable)
        then
            begin
            incr i;
            printf "// l: %d  pid: %d  f: %s@." !i (getpid ()) s
            end
        else ()


(* Miscellaneous **************************************************************)

let try_finally f x finally y =
    let result =
        try f x
        with exn -> finally y; raise exn
    in
    finally y;
    result

let relay_signal signal pid =
    let handler status =
        kill pid signal;
        printf "Pid to kill: %d@." pid;
        kill (getpid ()) signal;
        exit status
    in
    set_signal signal (Signal_handle handler)


let killall _ =
    List.iter (fun pid -> kill pid sigkill) !pid_list;
    exit 2

let mem_for_killall pid =
    pid_list := pid :: !pid_list

let rec rec_connect sock addr =
    try connect sock addr
    with _ -> rec_connect sock addr

let inet_addr_of_sockaddr sock =
    match sock with
    | ADDR_INET (inet_addr, _) -> inet_addr
    | _ -> assert false

let retransmit in_file_descr out_file_descr : 'a =
    let in_chan = in_channel_of_descr in_file_descr in
    let out_chan = out_channel_of_descr out_file_descr in
    while (true) do
        let v = (from_channel in_chan : 'a) in
        to_channel out_chan (v : 'a) [Closures];
        flush out_chan
    done;
    (from_channel in_chan : 'a)

let find_computers config_file =
    let computers = ref [] in
    let in_chan = open_in config_file in
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

let choose_computers config_file n =
    let computers = find_computers config_file in
    List.iter (fun s -> printf "%s@." s) computers;
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
                let l3 = choose (n - 1) computers in
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
