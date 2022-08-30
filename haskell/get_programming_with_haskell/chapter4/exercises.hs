import Data.List

-- Code examples from the Chapter
names = [("Ian", "Curtis"),
         ("Bernard", "Sumner"),
         ("Peter", "Hook"),
         ("Stephen", "Morris")]

compareLastNames :: (String, String) -> (String, String) -> Ordering
compareLastNames name1 name2 = if lastName1 > lastName2 then
                                 GT
                               else if lastName1 < lastName2 then
                                      LT
                                    else
                                      EQ
  where lastName1 = snd name1
        lastName2 = snd name2

addressLetter :: (String, String) -> String -> String
addressLetter name location = nameText ++ " - " ++ location
  where nameText = (fst name) ++ " " ++ (snd name)

sfOffice :: (String, String) -> String
sfOffice name = if lastName < "L"
                   then nameText
                        ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ lastName

nyOffice :: (String, String) -> String
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice :: (String, String) -> String
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

getLocationFunction :: String -> ((String, String) -> String)
getLocationFunction location = case location of
                                 "ny" -> nyOffice
                                 "sf" -> sfOffice
                                 "reno" -> renoOffice
                                 _ -> (\name -> (fst name) ++ "  " ++ (snd name))

addressLetter2 :: (String, String) -> String -> String
addressLetter2 name location = locationFunction name
  where locationFunction = getLocationFunction location

-- Exercises
compareLastNames2 :: (String, String) -> (String, String) -> Ordering
compareLastNames2 name1 name2 = compare lastName1 lastName2
  where lastName1 = snd name1
        lastName2 = snd name2

dcOffice :: (String, String) -> String
dcOffice name = nameText ++ " Esq."
  where nameText = (fst name) ++ " " ++ (snd name)
