generic
type T is private;

package Element is

  procedure Set (E : T);
  procedure Reset;
  function Get return T;
  function Is_Valid return Boolean;

  Invalid_Element : exception;

  private
  Value : T;
  Valid : Boolean := False;
end Element;
