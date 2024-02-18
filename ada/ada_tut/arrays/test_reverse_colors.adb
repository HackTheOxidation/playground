with Ada.Text_IO; use Ada.Text_IO;

with Colors; use Colors;

procedure Test_Reverse_Colors is
          My_Colors : Color_Array (1 .. 5) :=
                    (Black, Red, Green, Blue, White);
begin
        for C of My_Colors loop
            Put_Line("My_Color: " & Color'Image(C));
        end loop;

        New_Line;
        Put_Line("Reversing My_Color...");
        New_Line;
        Reverse_It(My_Colors);

        for C of My_Colors loop
            Put_Line("My_Color: " & Color'Image(C));
        end loop;
end Test_Reverse_Colors;
