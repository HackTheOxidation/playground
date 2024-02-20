procedure Generic_Swap (X, Y : in out T) is
  Tmp : constant T := X;
begin
  X := Y;
  Y := Tmp;
end Generic_Swap;
