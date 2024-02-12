from abc import ABC
from dataclasses import dataclass


class Message(ABC):
    pass


@dataclass(eq=True, frozen=True)
class Var(Message):
    message: str


@dataclass(eq=True, frozen=True)
class Const(Message):
    message: str

@dataclass(eq=True, frozen=True)
class CryptoMessage(Message):
    key: Message
    message: Message

    def __iter__(self):
        yield from (self.key, self.message)

class Crypt(CryptoMessage):
    pass


class Scrypt(CryptoMessage):
    pass


@dataclass(eq=True, frozen=True)
class Pair(Message):
    first: Message
    second: Message

    def __iter__(self):
        yield from (self.first, self.second)


@dataclass(eq=True, frozen=True)
class Inv(Message):
    message: Message


def dolev_yao_c(knowledge: set[Message], m: Message) -> bool:
    '''
    Restricted Dolev-Yao model (composition).
    '''
    if m in knowledge:
        return True

    if any(isinstance(m, cls) for cls in (Crypt, Scrypt, Pair)):
        return all(dolev_yao_c(knowledge, msg) for msg in m)

    return False


def analyse_imp(new: set[Message]) -> set[Message]:
    '''
    Imperative implementation of the knowledge analysis algorithm.
    '''
    hold = set()
    done = set()

    while new and (n := new.pop()):
        if isinstance(n, CryptoMessage):
            k, m = n
            k = Inv(k) if isinstance(n, Crypt) and not isinstance(k, Inv) else k
            if dolev_yao_c(new.union(hold).union(done), k):
                new = new.union(hold).union((m,))
                done.add(n)
                hold = set()
            else:
                hold.add(n)
            continue
        elif isinstance(n, Pair):
            new = new.union(hold).union(*n)
            hold = set()
        done.add(n)

    return hold.union(done)
        

def analyse(knowledge: set[Message]) -> set[Message]:
    '''
    Recursive/functional implementation of the knowledge analysis algorithm.
    '''

    def _analyse(new, hold, done) -> set[Message]:
        '''
        Recursive helper function.
        '''
        if not new:
            return hold.union(done)

        match (n := new.pop()):
            case Crypt(key=Inv(message=k), message=m):
                new.add(m)
                done.add(n)
                return _analyse(new.union(hold), set(), done)
            case Crypt(key=k, message=m) | Scrypt(key=k, message=m):
                k = Inv(k) if isinstance(n, Crypt) else k
                if dolev_yao_c(new.union(hold).union(done), k):
                    new.add(m)
                    done.add(n)
                    return _analyse(new.union(hold), set(), done)
                else:
                    hold.add(n)
                    return _analyse(new, hold, done)
            case Pair(first=m0, second=m1):
                done.add(n)
                return _analyse(set((m0, m1)).union(new).union(hold), set(), done)
            case _:
                done.add(n)
                return _analyse(new, hold, done)
                
    return _analyse(knowledge, set(), set())


def dolev_yao(knowledge: set[Message], m: Message) -> bool:
    '''
    Full (unrestricted) Dolev-Yao model.
    '''
    return dolev_yao_c(analyse_imp(knowledge), m)


if __name__ == '__main__':
    testknow = {
        Scrypt(Pair(Const("k1"), Const("k2")), Const("secret")),
        Scrypt(Const("k1"), Const("k2")),
        Const("k1"),
        }

    print("Test 1 - Running the restricted Dolev-Yao (composition) model...")
    test1 = dolev_yao_c(testknow, Const("secret"))
    print(f"Result: {test1}\n")

    print("Test 2 - Running the full Dolev-Yao model...")
    test2 = dolev_yao(testknow, Const("secret"))
    print(f"Result: {test2}")
