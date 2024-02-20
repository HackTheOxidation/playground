with Ada.Text_IO; use Ada.Text_IO;

procedure Decimal_Fixed_Point_Types is
   type T3_D3 is delta 10.0**(-3) digits 3;
   type T6_D3 is delta 10.0**(-3) digits 6;
begin
   Put_Line ("The delta value of T3_D3 is " & T3_D3'Image (T3_D3'Delta));
   Put_Line ("The minimum value of T3_D3 is " & T3_D3'Image (T3_D3'First));
   Put_Line ("The maximum value of T3_D3 is " & T3_D3'Image (T3_D3'Last));
   New_Line;

   Put_Line ("The delta value of T6_D3 is " & T6_D3'Image (T6_D3'Delta));
   Put_Line ("The minimum value of T6_D3 is " & T6_D3'Image (T6_D3'First));
   Put_Line ("The maximum value of T6_D3 is " & T6_D3'Image (T6_D3'Last));
end Decimal_Fixed_Point_Types;
