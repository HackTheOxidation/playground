type 'a node =
  | One of 'a
  | Many of 'a node list

let rec count all counter current = function
  | [] -> all@[(counter, current)]
  | x::xs ->
      if x == current then
        count all (counter + 1) current xs
      else
        count (all@[(counter, current)]) 1 x xs

let decode = function
  | [] -> []
  | x::xs ->
      let counted = count [] 1 x xs
      List.map (fun (c, x) -> if c == 1 then One x else Many (c, x)) counted
