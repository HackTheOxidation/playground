#include <string>
#include <type_traits>
#include <utility>

class Person {
public:
    std::string name_;
    int age_;

    template<std::size_t Index>
    std::tuple_element_t<Index, Person>& get() {
        if constexpr(Index == 0) return name_;
        if constexpr(Index == 1) return age_;
    }
};

namespace std {
    template<>
    struct tuple_size<::Person> {
        static constexpr size_t value = 2;
    };

    template<>
    struct tuple_element<0, ::Person> {
        using type = std::string;
    };

    template<>
    struct tuple_element<1, ::Person> {
        using type = int;
    };

    template<size_t Index>
    struct tuple_element<Index, ::Person>
        : conditional<Index == 0, std::string, int> {
            static_assert(Index < 2, "Index out of bounds for Person");
    };
};

int main(void) {
    Person p;

    auto&& [name, age] = p;
    name = "Fred";
    age = 42;

    return 0;
}
