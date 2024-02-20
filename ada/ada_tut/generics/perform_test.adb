with Ada.Text_IO; use Ada.Text_IO;

procedure Perform_Test (X : in out Array_T) is
begin
  for C of X loop
    Put_Line(S & ": " & Image(C));
  end loop;

  New_Line;
  Put_Line ("Testing " & S & "...");
  New_Line;
  Test(X);

  for C of X loop
    Put_Line(S & ": " & Image(C));
  end loop;
end Perform_Test;
