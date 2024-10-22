#pragma once

#include "static.hh"
#include "sub_array.hh"

namespace geometry {

template<class T, std::size_t MaxSize>
class set {
public:
    static constexpr std::size_t max_size{MaxSize};
    using storage = sub_array<T, max_size>;

private:
    storage _elements;

public:
    set(std::convertible_to<T> auto&&... elements) requires (sizeof...(elements) <= max_size) :
        _elements{elements...}
    {}

    set(storage elements) :
        _elements{elements}
    {}

    T const* begin() const {
        return _elements.begin();
    }

    T const* end() const {
        return _elements.end();
    }

    static void show_type(auto& stream) {
        stream.write("{");
        if constexpr (max_size > 0) {
            show(stream, proxy<T>{});
        }
        stream.write("}");
    }

    friend void show(auto& stream, set const& value) {
        stream.write("{");
        show_range(stream, " ", value._elements.begin(), value._elements.end());
        stream.write("}");
    }
};

using empty = set<univ, 0>;

set() -> set<univ, 0>;

template<class... Ts>
set(Ts...) -> set<std::common_type_t<Ts...>, sizeof...(Ts)>;

template<class T>
concept is_set =
    decltype(overload{
        []<class Elem, std::size_t MaxSize>(set<Elem, MaxSize> const&) { return std::true_type{}; },
        [](auto const&) { return std::false_type{}; }
    }(std::declval<T>()))::value;

template<class T, std::size_t X>
using enlarge_set =
    decltype([]<class Elem, std::size_t MaxSize>(set<Elem, MaxSize>)
        -> set<Elem, MaxSize * X> {}
        (std::declval<T>()));

}
