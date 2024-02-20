generic
type T is private;
type Index is range <>;
type Array_T is array (Index range <>) of T;
S : String;
with function Image (E : T) return String is <>;
with procedure Test (X : in out Array_T);

procedure Perform_Test (X : in out Array_T);
