open Unix

(* Constant *******************************************************************)

let trace_enable = false


(* Trace function *************************************************************)

let trace =
    let i = ref 0 in
    fun s ->
        if (trace_enable)
        then
            begin
            incr i;
            Format.printf "// l: %d  pid: %d  f: %s@." !i (getpid ()) s
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
