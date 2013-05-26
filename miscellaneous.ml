open Format
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
    List.iter (fun s -> Format.printf "##### %d @." s) !pid_list;
    List.iter (fun pid -> kill pid sigkill) !pid_list;
    exit 2

let mem_for_killall pid =
    pid_list := pid :: !pid_list
