#pragma once

#include <variant>

#include "format.hh"

namespace geometry {

template<class... Ts>
class oneof;

template<class... Ts>
using oneof_unique = unique_pack_t<oneof, Ts...>;

template<class... Ts>
class oneof {
private:
    std::variant<Ts...> _value;

public:
    template<class U>
    oneof(U&& value) requires (std::convertible_to<Ts, U> || ...) :
        _value{value}
    {}

    auto match(auto&& handle) const {
        return std::visit(std::forward<decltype(handle)>(handle), _value);
    }

    auto match(auto&& handle) && {
        return std::visit(std::forward<decltype(handle)>(handle), std::move(_value));
    }

    static void show_type(auto& stream) {
        stream.write("<");
        show_many(stream, " | ", proxy<Ts>{}...);
        stream.write(">");
    }

    friend void show(auto& stream, oneof const& v) {
        std::visit([&]<class T>(T const& obj) {
            show(stream, proxy<T>{});
            stream.write(":");
            show(stream, obj);
        }, v._value);
    }
};

}
