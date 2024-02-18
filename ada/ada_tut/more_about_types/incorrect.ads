package Incorrect is
        type Point is record
             X, Y : Integer := 0;
        end record;

        -- Error! Y is not specified!
        Origin : Point := (X => 0);
end Incorrect;
