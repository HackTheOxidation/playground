from abc import ABC
from copy import deepcopy
from dataclasses import dataclass


@dataclass(eq=True, frozen=True)
class Message(ABC):
    pass


@dataclass(eq=True, frozen=True)
class Var(Message):
    message: str


@dataclass(eq=True, frozen=True)
class Const(Message):
    message: str


@dataclass(eq=True, frozen=True)
class Crypt(Message):
    key: Message
    message: Message


@dataclass(eq=True, frozen=True)
class Scrypt(Message):
    key: Message
    message: Message


@dataclass(eq=True, frozen=True)
class Pair(Message):
    first: Message
    second: Message


@dataclass(eq=True, frozen=True)
class Inv(Message):
    message: Message


def dolev_yao_c(knowledge: set[Message], m: Message) -> bool:
    '''
    Restricted Dolev-Yao model (composition).
    '''
    if m in knowledge:
        return True

    match m:
        case Crypt(key=k, message=m0) | Scrypt(key=k, message=m0):
            return all(dolev_yao_c(knowledge, m) for m in (k, m0))
        case Pair(first=m0, second=m1):
            return all(dolev_yao_c(knowledge, m) for m in (m0, m1))
        case _:
            return False


def analyse_imp(knowledge: set[Message]) -> set[Message]:
    '''
    Imperative implementation of the knowledge analysis algorithm.
    '''
    new = knowledge
    hold = set()
    done = set()

    def update_crypt(m, n):
        new.add(m)
        done.add(n)
        new = new.union(hold)
        hold = set()
        
    while new:
        match (n := new.pop()):
            case Crypt(key=Inv(message=k), message=m):
                update_crypt(m, n)
            case Crypt(key=k, message=m) | Scrypt(key=k, message=m):
                k = Inv(k) if isinstance(n, Crypt) else k
                if dolev_yao_c(new.union(hold).union(done), k):
                    update_crypt(m, n)
                else:
                    hold.add(n)
            case Pair(first=m0, second=m1):
                new = set((m0, m1)).union(new).union(hold)
                hold = set()
                done.add(n)
            case _:
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
    return dolev_yao_c(analyse(knowledge), m)


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
