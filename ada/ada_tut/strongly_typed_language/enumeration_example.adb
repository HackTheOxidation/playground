with Ada.Text_IO; use Ada.Text_IO;

procedure Enumeration_Example is
          type Days is (Monday, Tuesday, Wednesday,
                        Thursday, Friday,
                        Saturday, Sunday);
begin
        for I in Days loop
            case I is
                 when Saturday .. Sunday =>
                      Put_Line("Week end!");

                 when Monday .. Friday =>
                      Put_Line("Hello on " & Days'Image(I));
            end case;
        end loop;
end Enumeration_Example;
