with Ada.Text_IO; use Ada.Text_IO;

procedure Show_Decimal_IO_Inst is

  type Price is delta 10.0 ** (-2) digits 12;

  package Price_IO is new Ada.Text_IO.Decimal_IO (Price);

  P : Price;
begin

  Price_IO.Default_Exp := 0;

  P := 2.5;
  Price_IO.Put (P);
  New_Line;

  P := 5.75;
  Price_IO.Put (P);
  New_Line;
end Show_Decimal_IO_Inst;
