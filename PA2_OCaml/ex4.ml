module F = Format

let rec gcd a b =
        match a, b with
        | _, 0 -> a
        | _, _ -> gcd b (a mod b)

let phi m = 
        match m with
        | 1 -> 1
        | _ -> let rec count_coprimes cnt n =
                if n = m then cnt
                else if gcd m n = 1 then count_coprimes (cnt + 1) (n + 1)
                else count_coprimes cnt (n + 1)
               in
               count_coprimes 0 1


let _ =
        let _ = F.printf "%d\n" (phi 4) in
        let _ = F.printf "%d\n" (phi 9) in
        let _ = F.printf "%d\n" (phi 10) in
        let _ = F.printf "%d\n" (phi 17) in
        F.printf "%d\n" (phi 30)

