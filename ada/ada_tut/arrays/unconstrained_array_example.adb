with Ada.Text_IO; use Ada.Text_IO;

procedure Unconstrained_Array_Example is
          type Days is (Monday, Tuesday, Wednesday,
                        Thursday, Friday,
                        Saturday, Sunday);

          type Workload_Type is
               array (Days range <>) of Natural;

          Workload : constant Workload_Type (Monday .. Friday) :=
                   (Friday => 7, others => 8);
begin
        for I in Workload'Range loop
            Put_Line(Integer'Image(Workload(I)));
        end loop;
end Unconstrained_Array_Example;
