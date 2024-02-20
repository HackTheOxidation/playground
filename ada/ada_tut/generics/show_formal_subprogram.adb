with Check;

procedure Show_Formal_Subprogram is
  A, B : Integer;

  procedure Check_Is_Equal is 
    new Check (Description => "equality", T => Integer, Comparison => Standard."=");
begin
  A := 0;
  B := 1;
  Check_Is_Equal (A, B);
end Show_Formal_Subprogram;
