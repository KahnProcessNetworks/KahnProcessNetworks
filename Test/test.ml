open Kahn


module Lib (K : S) =
struct
    let ( >>= ) x f =
        K.bind x f
    let delay f x =
        (K.return ()) >>= (fun () -> K.return (f x))
end

module Example (K : S) =
struct
    module Lib = Lib(K)
    open Lib
    
    let pa (qo1 : int K.out_port) (qo2 : float K.out_port) : unit K.process =
        let rec loop n =
            (K.put n qo1) >>= (fun () -> ( (K.put ((float_of_int n)/.10.) qo2) >>= (fun () -> loop (n+1)) ) )
        in
        loop 2
    
    let pb (qi1 : int K.in_port) (qi2 : float K.in_port): unit K.process =
        let rec loop () =
            (K.get qi2) >>= (fun (v:float) -> Format.printf "%f@." v; K.get qi1 >>=(  fun (j:int) -> Format.printf "%d@." j; loop()    ))
        in
        loop ()
    
    let main () : unit K.process =
        (delay K.new_channel ()) >>= (fun (q_in1, q_out1) ->  (delay K.new_channel ()) >>= fun (q_in2, q_out2) ->K.doco [ pa q_out1 q_out2 ; pb q_in1 q_in2 ; ])
end
 
module Exp = Example(Socket) 

let () = Socket.run(Exp.main ())
