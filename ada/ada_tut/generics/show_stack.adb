with Ada.Text_IO; use Ada.Text_IO;
with Stacks;

procedure Show_Stack is

  package Integer_Stacks is 
    new Stacks (Max => 10, T => Integer);
  use Integer_Stacks;

  Values : Integer_Stacks.Stack;
begin
  Push (Values, 10);
  Push (Values, 20);

  Put_Line("Last value was " & Integer'Image(Pop(Values)));
end Show_Stack;
