module F = Format

let palindrome lst =
        let rec is_palindrome i1 i2 =
                if i1 >= i2 then true
                else if List.nth lst i1 <> List.nth lst i2 then false
                else is_palindrome (i1 + 1) (i2 - 1)
                in 
                is_palindrome 0 (List.length lst - 1)

let _ =
        let _ = F.printf "%b\n" (palindrome ["1"; "2"; "3"; "4"]) in
        let _ = F.printf "%b\n" (palindrome ["x"; "m"; "a"; "s"]) in
        let _ = F.printf "%b\n" (palindrome ["a"; "m"; "o"; "r"; "e"; "r"; "o"; "m"; "a"]) in
        let _ = F.printf "%b\n" (palindrome ["1"; "2"; "3"; "2"; "1"]) in
        F.printf "%b\n" (palindrome ["b"; "o"; "r"; "r"; "o"; "w"; "o"; "r"; "r"; "o"; "b"])

