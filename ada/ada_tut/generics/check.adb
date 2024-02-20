with Ada.Text_IO; use Ada.Text_IO;

procedure Check (X, Y : T) is
  Result : Boolean;
begin
  Result := Comparison (X, Y);
  if Result then
    Put_Line ("Comparison (" & Description & ") between arguments is OK!");
  else
    Put_Line ("Comparison (" & Description & ") between arguments is not OK!");
  end if;
end Check;
