with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
          type My_Int is range 1 .. 20;
          A : My_Int := 12;
          B : My_Int := 20;
          M : My_Int := (A + B) / 2;

          type Mod_Int is mod 2 ** 5;
begin
        for I in 1 .. M loop
            Put_Line("Hello, World!");
        end loop;
end Main;
