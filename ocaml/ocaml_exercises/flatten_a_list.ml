
type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten_inner acc = function
  | [] -> List.rev acc
  | x::xs -> 
      match x with
      | One v -> flatten_inner (v::acc) xs
      | Many vs -> flatten_inner ((flatten_inner [] vs)@acc) xs

let flatten nodes =
  flatten_inner [] nodes

