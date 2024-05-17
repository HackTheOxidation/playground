let rec compress_inner acc c = function
  | [] -> List.rev acc
  | x::xs -> compress_inner (if x = c then acc else x::acc) x xs

let compress = function
  | [] -> []
  | x::xs -> compress_inner [x] x xs

