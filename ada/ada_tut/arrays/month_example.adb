with Ada.Text_IO; use Ada.Text_IO;

procedure Month_Example is
          type Month_Duration is range 1 .. 31;
          type Month is (Jan, Feb, Mar, Apr,
                         May, Jun, Jul, Aug,
                         Sep, Oct, Nov, Dec);

          type My_Int_Array is array (Month) of Month_Duration;

          Tab : constant My_Int_Array :=
              (31, 28, 31, 30, 31, 30,
               31, 31, 30, 31, 30, 31);

          Feb_Days : Month_Duration := Tab(Feb);
begin
        for M in Month loop
            Put_Line(Month'Image(M) & " has "
                     & Month_Duration'Image(Tab(M))
                     & " days.");
        end loop;
end Month_Example;
