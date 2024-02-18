package Colors is
        type Color is (Black, Red, Green, Blue, White);

        type Color_Array is array (Positive range <>) of Color;

        procedure Reverse_It (X : in out Color_Array);
end Colors;
