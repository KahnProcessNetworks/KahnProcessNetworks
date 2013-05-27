open Kahn


module Lib (K : S) =
struct
    let ( >>= ) x f =
        K.bind x f
    let delay f x =
        (K.return ()) >>= (fun () -> K.return (f x))
end

module Eratosthene (K : S) =
struct
    module Lib = Lib(K)
    open Lib
    
    let integers (qo : int K.out_port) : unit K.process =
        let rec loop n =
            (K.put n qo) >>= (fun () -> loop (n + 1))
        in
        loop 2
    
    (* TODO: Filter et sift sont à améliorer. Cf. TP 5. *)
    
    let filter prime (qi : int K.in_port) (qo : int K.out_port) : unit K.process =
        let rec loop () =
            (K.get qi) >>= (fun v -> if (0 <> v mod prime) then K.put n qo;)
        in
        loop ()
    
    let rec sift (qi : int K.in_port) (qo : int K.out_port) : unit K.process =
        (* TODO: Il faut surement utiliser >>= pour lier le get au put puis au doco. *)
        let prime = K.get qi;
        K.put prime qo;
        (delay K.new_channel ()) >>= (fun (q_in, q_out) -> K.doco [ filter prime qi q_in; sift q_out qo; ]
    
    let output (qi : int K.in_port) : unit K.process =
        let rec loop () =
            (K.get qi) >>= (fun v -> Format.printf "%d@." v; loop ())
        in
        loop ()
    
    (* TODO: Trouver la syntaxe pour créer deux channels en même temps. *)
    let main () : unit K.process =
        (delay K.new_channel ()) >>= (fun (qi, qo) -> K.doco [ integers qo1; sift qi1 qo2; output qi2; ])
end
 
module Era = Eratosthene(S) 

let () = S.run(Exp.main ())
