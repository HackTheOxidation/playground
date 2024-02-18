package body Colors is
        procedure Reverse_It (X : in out Color_Array) is
        begin
                for I in X'First .. (X'Last + X'First) / 2 loop
                    declare
                        Tmp : Color:
                        X_Left : Color renames X(I);
                        X_Right : Color renames X(X'Last + X'First - I);
                    begin
                        Tmp := X_Left;
                        X_Left := X_Right;
                        X_Right := Tmp;
                    end;
                end loop;
        end Reverse_It;
end Colors;
