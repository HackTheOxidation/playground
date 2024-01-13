let rec compress_inner acc c = function
  | [] -> List.rev acc
  | x::xs ->
      if x == c then
        compress_inner acc c xs
      else
        compress_inner (x::acc) x xs

let compress = function
  | [] -> []
  | x::xs ->
      compress_inner [x] x xs



