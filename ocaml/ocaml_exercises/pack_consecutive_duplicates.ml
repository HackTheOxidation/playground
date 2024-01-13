let rec pack_inner all current c = function
  | [] -> all @ [current]
  | x::xs ->
      if x == c then
        pack_inner all (x::current) c xs
      else
        pack_inner (all @ [current]) [x] x xs

let pack = function
  | [] -> []
  | x::xs ->
      pack_inner [] [x] x xs
