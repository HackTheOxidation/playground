procedure Generic_Reverse (X : in out Array_T) is
begin
  for I in X'First .. (X'Last + X'First) / 2 loop
    declare
      X_Left : T renames X (I);
      X_Right : T renames X (X'Last + X'First - I);
      Tmp : constant T := X_Left;
    begin
      X_Left := X_Right;
      X_Right := Tmp;
    end;
  end loop;
end Generic_Reverse;
