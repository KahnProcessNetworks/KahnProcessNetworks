open Unix

(* Constant *******************************************************************)

let trace_enable = true


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
