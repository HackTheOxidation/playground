with Ada.Text_IO; use Ada.Text_IO;
with Element;

procedure Show_Generic_Package is

  package I is new Element (T => Integer);

  procedure Display_Initialized is
  begin
    if I.Is_Valid then
      Put_Line("Value is initialized");
    else
      Put_Line("Value is not initialized");
    end if;
  end Display_Initialized;

begin
  Display_Initialized;

  Put_Line("Initializing...");
  I.Set(5);
  Display_Initialized;
  Put_Line("Value is now set to " & Integer'Image(I.Get));

  Put_Line("Resetting...");
  I.Reset;
  Display_Initialized;

end Show_Generic_Package;
