package My_Types is 
   
   type Priv_Rec is private;

   private

   type Priv_Rec is record
      Number : Integer := 42;
   end record;

end My_Types;
