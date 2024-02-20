with Ada.Text_IO; use Ada.Text_IO;

package body Encapsulate.Child is

   procedure Hello3 is
   begin
      Hello2;
      Put_Line ("Hello #3");
   end Hello3;

end Encapsulate.Child;
