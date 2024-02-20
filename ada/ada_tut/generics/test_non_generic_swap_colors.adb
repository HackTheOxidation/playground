with Ada.Text_IO; use Ada.Text_IO;
with Colors; use Colors;

procedure Test_Non_Generic_Swap_Colors is
  A, B, C : Color;
begin
  A := Blue;
  B := White;
  C := Red;

  Put_Line("Value of A is " & Color'Image(A));
  Put_Line("Value of B is " & Color'Image(B));
  Put_Line("Value of C is " & Color'Image(C));

  New_Line;
  Put_Line("Swapping A and C...");
  New_Line;
  Swap_Colors(A, C);

  Put_Line("Value of A is " & Color'Image(A));
  Put_Line("Value of B is " & Color'Image(B));
  Put_Line("Value of C is " & Color'Image(C));
end Test_Non_Generic_Swap_Colors;
