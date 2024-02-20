package Variant_Record is
   type Expr;

   type Expr_Access is access Expr;

   type Expr_Kind_Type is (Bin_Op_Plus, Bin_Op_Minus, Num);

   type Expr (Kind : Expr_Kind_Type) is record
      case Kind is
         when Bin_Op_Plus | Bin_Op_Minus =>
            Left, Right : Expr_Access;
         when Num =>
            Val : Integer;
      end case;
   end record;
end Variant_Record;
