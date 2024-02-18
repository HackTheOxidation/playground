with Ada.Text_IO; use Ada.Text_IO;

procedure Simple_Array_Bounds is
          type My_Int is range 0 .. 1000;
          type My_Int_Array is array (1 .. 5) of My_Int;
          Tab : constant My_Int_Array := (2, 3, 5, 7, 11);
begin
        for I in 1 .. 5 loop
            Put(My_Int'Image(Tab(I)));
        end loop;
        New_Line;
end Simple_Array_Bounds;
