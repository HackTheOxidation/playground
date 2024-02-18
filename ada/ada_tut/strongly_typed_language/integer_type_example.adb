with Ada.Text_IO; use Ada.Text_IO;

procedure Integer_Type_Example is
          type My_Int is range -1 .. 20;
begin
        for I in My_Int loop
            Put_Line (My_Int'Image(I));
        end loop;
end Integer_Type_Example;
