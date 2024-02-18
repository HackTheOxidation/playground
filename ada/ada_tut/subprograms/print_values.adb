with Ada.Text_IO; use Ada.Text_IO;

procedure Print_Values
          (A, B, C : Integer) is
begin
        Put_Line("Increment of "
                            & Integer'Image(A)
                            & " with "
                            & Integer'Image(B)
                            & " is "
                            & Integer'Image(C));
end Print_Values;
