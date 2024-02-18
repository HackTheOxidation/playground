with Ada.Text_IO; use Ada.Text_IO;

procedure Greet is
          type Days is (Monday, Tuesday, Wednesday
                        Thursday, Friday,
                        Saturday, Sunday);

          subtype Weekend_Days is 
               Days range Saturday .. Sunday;

          M : Days := Sunday;
          S : Weekend_Days := M;
begin
        for I in Days loop
            case I is
                 when Weekend_Days =>
                      Put_Line("Week end!");
                 when others =>
                      Put_Line("Hello on " & Days'Image(I));
            end case;
        end loop;
end Greet;
