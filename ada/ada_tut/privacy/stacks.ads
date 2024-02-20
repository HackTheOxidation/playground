package Stacks is
   type Stack is private;

   procedure Push (S : in out Stack; Val : Integer);

   procedure Pop (S : in out Stack; Val : out Integer);

private
   subtype Stack_Index is Natural range 1 .. 10;

   type Content_Type is array (Stack_Index) of Natural;

   type Stack is record
      Top     : Stack_Index;
      Content : Content_Type;
   end record;
end Stacks;
