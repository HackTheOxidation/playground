with Colors; use Colors;
with Perform_Test;

procedure Test_Reverse_Colors is

  procedure Perform_Test_Reverse_It is
    new Perform_Test (T => Color, Index => Integer, Array_T => Color_Array,
                      S => "My_Color", Image => Color'Image, Test => Reverse_It);

  My_Colors : Color_Array (1 .. 5) := (Black, Red, Green, Blue, White);
begin
  Perform_Test_Reverse_It(My_Colors);
end Test_Reverse_Colors;
