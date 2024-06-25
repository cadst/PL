module F = Format

let is_prime n =
  let rec i d =
          if d * d > n then true
          else if n mod d = 0 then false
          else i (d+1)
  in
  n > 1 && i 2

let rec prime n =
  if is_prime n then n else prime (n + 1)

let goldbach n =
  let rec g d =
    if is_prime d && is_prime (n - d) && d <= n - d then (d, n - d)
    else if is_prime d && is_prime (n-d) then (n - d, d)
    else if n > d then g (prime (d + 1))
    else (0, 0) 
  in
  g 2

let rec goldbach_list_limit lower upper limit =
   let n = lower in
        if n > upper then []
        else
                let gb_list = goldbach n in
                if fst gb_list >= limit then
                     (n, gb_list) :: goldbach_list_limit (lower + 1) upper limit
                else
                     goldbach_list_limit (lower + 1) upper limit 


let print_gb lower upper limit=
        let gll = goldbach_list_limit lower upper limit in 
        let gb =
                List.map (fun (f1, (f2, f3)) -> F.sprintf "(%d, (%d, %d))" f1 f2 f3) gll in
                "[" ^ String.concat "; " gb ^ "]"

let _ =
        let _ = F.printf "%s\n" (print_gb 9 20 5) in
        let _ = F.printf "%s\n" (print_gb 25 70 10) in
        let _ = F.printf "%s\n" (print_gb 100 100 100) in
        let _ = F.printf "%s\n" (print_gb 100 200 19) in
        let _ = F.printf "%s\n" (print_gb 50 500 20) in
        F.printf "%s\n" (print_gb 1 2000 50) 
        

