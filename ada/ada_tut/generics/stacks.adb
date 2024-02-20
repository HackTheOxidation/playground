package body Stacks is

  function Is_Empty (S : Stack) return Boolean is
    (S.Top < S.Container'First);

  function Is_Full (S : Stack) return Boolean is
    (S.Top >= S.Container'Last);

  function Pop (S : in out Stack) return T is
  begin
    if Is_Empty (S) then
      raise Stack_Underflow;
    else
      return X : T do
        X := S.Container (S.Top);
        S.Top := S.Top - 1;
      end return;
    end if;
  end Pop;

  procedure Push (S : in out Stack; V : T) is
  begin
    if Is_Full (S) then
      raise Stack_Overflow;
    else
      S.Top := S.Top + 1;
      S.Container (S.Top) := V;
    end if;
  end Push;

end Stacks;
