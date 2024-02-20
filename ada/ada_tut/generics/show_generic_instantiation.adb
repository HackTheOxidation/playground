with Ada.Text_IO; use Ada.Text_IO;
with Set;

procedure Show_Generic_Instantiation is

  Main : Integer := 0;
  Current : Integer;

  procedure Set_Main is new Set (T => Integer, X => Main);

begin
  Current := 10;

  Set_Main(Current);
  Put_Line("Value of Main is " & Integer'Image(Main));

end Show_Generic_Instantiation;
