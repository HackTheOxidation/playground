generic
Max : Positive;
type T is private;

package Stacks is

  type Stack is limited private;

  Stack_Underflow, Stack_Overflow : exception;

  function Is_Empty (S : Stack) return Boolean;

  function Pop (S : in out Stack) return T;

  procedure Push (S : in out Stack; V : T);

  private

  type Stack_Array is array (Natural range <>) of T;

  Min : constant := 1;

  type Stack is record
    Container : Stack_Array (Min .. Max);
    Top : Natural := Min - 1;
  end record;

end Stacks;
