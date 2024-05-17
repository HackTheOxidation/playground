let drop xs idx =
  List.filteri (fun i _ -> (i + 1) mod idx <> 0) xs
