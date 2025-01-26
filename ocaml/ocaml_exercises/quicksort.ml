let rec qsort = function
  | [] -> []
  | x::xs ->
      let smaller = List.filter (fun n -> n <= x) xs
      let larget = List.filter (fun n -> n > x) xs
      qsort smaller @ [x] @ qsort larger
