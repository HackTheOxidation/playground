with Ada.Text_IO; use Ada.Text_IO;

procedure Show_Days is
          type Days is (Monday, Tuesday, Wednesday,
                        Thursday, Friday,
                        Saturday, Sunday);

          subtype Day_Name is String (1 .. 2);

          type Days_Name_Type is array (Days) of Day_Name;

          Names : constant Days_Name_Type :=
                ("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su");
begin
        for I in Names'Range loop
            Put_Line(Names(I));
        end loop;
end Show_Days;
