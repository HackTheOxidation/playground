let slice xs start stop = 
  List.filteri (fun i _ -> i >= start && i <= stop) xs
