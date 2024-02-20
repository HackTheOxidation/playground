with Ada.Text_IO; use Ada.Text_IO;

package body My_Types.Ops is

   procedure Display (E : Priv_Rec) is
   begin
      Put_Line ("Priv_Rec.Number: " & Integer'Image (E.Number));
   end Display;

end My_Types.Ops;
