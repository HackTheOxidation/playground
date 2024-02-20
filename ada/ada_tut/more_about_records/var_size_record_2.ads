package Var_Size_Record_2 is
        type Items_Array is array (Positive range <>) of Integer;

        type Growable_Stack (Max_Len : Natural) is record
                Items : Items_Array (1 .. Max_Len);
                Len   : Natural := 0;
        end record;

end Var_Size_Record_2;
