with Ada.Text_IO;       use Ada.Text_IO;
with Var_Size_Record_2; use Var_Size_Record_2;

procedure Main is
        procedure Print_Stack (G : Growable_Stack) is
        begin
                Put ("<Stack, items: [");
                for I in G.Items'Range loop
                        exit when I > G.Len;
                        Put (" " & Integer'Image (G.Items (I)));
                end loop;
                Put_Line ("]>");
        end Print_Stack;
        S : Growable_Stack :=
               (Max_Len => 128, Items => (1, 2, 3, 4, others => <>), Len => 4);
begin
        Print_Stack (S);
end Main;
