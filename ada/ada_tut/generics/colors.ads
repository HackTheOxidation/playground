with Generic_Reverse;
with Generic_Swap;

package Colors is
  type Color is (Black, Red, Green, Blue, White);

  procedure Swap_Colors is new Generic_Swap (T => Color);

  type Color_Array is array (Integer range <>) of Color;

  procedure Reverse_It is new Generic_Reverse (T => Color, Index => Integer, Array_T => Color_Array);

end Colors;
