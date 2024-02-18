function Quadruple (I : Integer) return Integer is
         function Double (I : Integer) return Integer is
         begin
                return I * 2;
         end Double;

         Res : Integer := Double(Double(I));
begin
        return Res;
end Quadruple;
