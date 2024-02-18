with Ada.Text_IO; use Ada.Text_IO;

procedure Character_Example is
          type My_Char is ('a', 'b', 'c');

          C : Character;

          M : My_Char;
begin
        C := '?';

        M := 'a';

        C := Character'Val(65);
end Character_Example;
