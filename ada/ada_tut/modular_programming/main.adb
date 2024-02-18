with Ada.Text_IO;
with Week.Child; use Week.Child;
with Week.Child_2; use Week.Child_2;

with Week.Child.Grandchild;
use Week.Child.Grandchild;

procedure Main is
          package TIO renames Ada.Text_IO;
          procedure Say (Something : String)
                    renames Ada.Text_IO.Put_Line;
begin
        TIO.Put_Line("First day of the week is "
                 & Get_First_Of_Week);

        TIO.Put_Line("Second day of the week is "
                 & Get_Second_Of_Week);

        TIO.Put_Line("Last day of the week is "
                 & Get_Last_Of_Week);
end Main;
