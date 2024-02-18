with Ada.Text_IO; use Ada.Text_IO;

procedure GreetDeclare is
begin
        loop
                Put_Line("Please enter your name: ");

                declare
                        Name : String := Get_Line;
                begin
                        exit when Name = "";
                        Put_Line("Hi " & Name & "!");
                end;
        end loop;

        Put_Line("Bye!");
end GreetDeclare;
