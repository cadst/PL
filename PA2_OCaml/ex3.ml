module F = Format

let factor_list n =
        let rec f d n count = 
                if n = 1 then []
                else if n mod d = 0 then
                        let rec ff d n count =
                                if n mod d = 0 then ff d (n / d) (count + 1)
                                else (d, count) :: f (d+1) n 0 in
                       ff d n count else f (d+1) n 0 in f 2 n 0

let fibo n =
        let factors = factor_list n in
        let factor_f = 
                List.map (fun (factor, factor_n) -> F.sprintf "(%d, %d)" factor factor_n) factors in
        "[" ^ String.concat "; " factor_f ^ "]"

let _ =
        let _ = F.printf "%s\n" (fibo 10) in
        let _ = F.printf "%s\n" (fibo 17) in
        let _ = F.printf "%s\n" (fibo 27) in
        let _ = F.printf "%s\n" (fibo 315) in
        let _ = F.printf "%s\n" (fibo 777) in
        F.printf "%s\n" (fibo 1024)
