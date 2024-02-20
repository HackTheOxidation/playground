with Ada.Text_IO; use Ada.Text_IO;

with Variant_Record; use Variant_Record;

procedure Evaluator is
   function Eval_Expr (E : Expr) return Integer is
     (case E.Kind is
        when Bin_Op_Plus => Expr_Eval (E.Left.all) + Eval_Expr (E.Right.all),
        when Bin_Op_Minus => Expr_Eval (E.Left.all) - Eval_Expr (E.Right.all),
        when Num => E.Val);

   E : Expr :=
     (Bin_Op_Plus,
      new Expr'(Bin_Op_Minus, new Expr'(Num, 12), new Expr'(Num, 15)),
      new Expr'(Num, 3));
begin
   Put_Line (Integer'Image (Eval_Expr (E)));
end Evaluator;
