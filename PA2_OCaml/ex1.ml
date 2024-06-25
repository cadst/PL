module F = Format

let rec gcd a b = 
        match a, b with
        | _, 0 -> a
        | _, _ -> gcd b (a mod b)

let _ = 
        let _ = F.printf "%d\n" (gcd 10 0) in
        let _ = F.printf "%d\n" (gcd 9 5) in
        let _ = F.printf "%d\n" (gcd 13 13) in
        let _ = F.printf "%d\n" (gcd 37 600) in
        F.printf "%d\n" (gcd 0 0)
