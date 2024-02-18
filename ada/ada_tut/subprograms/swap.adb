procedure Swap (A, B : Integer) is
          Tmp : Integer;
begin
        Tmp := A;

        -- Error: A is "in" mode by default!
        A := B;

        -- Error: B is "in" mode by default!
        B := Tmp;
end Swap;
