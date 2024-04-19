with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets; 
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Unbounded_Priority_Queues;

procedure Breadth_First_Search is
  type Node;

  type Node_Access is access Node;

  function "=" (Left : Node; Right : Node) return Boolean;

  type Node is record
    Value : Integer;
    Child : Node_Access := null;
  end record;

  function "=" (Left, Right : Node) return Boolean is
  begin
    return Left.Value = Right.Value;
  end "=";

  function "<" (Left : Node; Right : Node) return Boolean is
  begin
    return Left.Value < Right.Value;
  end "<";

  function Equivalent_Keys (Left, Right : Node) return Boolean is
  begin
    return Left = Right;
  end Equivalent_Keys;

  function Hash (N : Node) return Hash_Type is
  begin
    return Hash_Type(N.Value);
  end Hash;

  package Node_Set is 
    new Ada.Containers.Ordered_Sets (
      Element_Type => Node);

  use Node_Set;

  package Edge_Map is
    new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type => Node,
      Element_Type => Node_Set,
      Hash => Hash,
      Equivalent_Keys => "=");

  use Edge_Map;

  Visited : Set;
begin
  null;
end Breadth_First_Search;
