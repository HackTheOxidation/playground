with Ada.Text_IO; use Ada.Text_IO;

procedure Nested is
          procedure NestedNested is
          begin
                Put_Line("Hello World");
          end NestedNested;
begin
        NestedNested;
end Nested;
