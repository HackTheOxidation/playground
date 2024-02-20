with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Be_Careful is
  function Dangerous return Integer is
  begin
    raise Constraint_Error;
    return 42;
  end Dangerous;

begin
  declare
    -- This will NOT be handled/caught.
    A : Integer := Dangerous;
  begin
    Put_Line(Integer'Image(A));
  exception
    when Constraint_Error =>
      Put_Line("error!");
  end;
end Be_Careful;
