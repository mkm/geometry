#pragma once

#include <string_view>
#include <tuple>

#include "format.hh"

namespace geometry {

template<std::size_t Value>
struct index {
    static constexpr std::size_t value = Value;
};

template<class SeqLeft, class SeqRight>
using CatSeq = decltype(
    []<std::size_t... IxLeft, std::size_t... IxRight>
    (std::index_sequence<IxLeft...>, std::index_sequence<IxRight...>) {
        return std::index_sequence<IxLeft..., IxRight...>{};
    }(std::declval<SeqLeft>(), std::declval<SeqRight>()));

template<std::size_t I, class... Ts>
auto select(Ts&&... args) {
    return std::get<I>(std::forward_as_tuple(args...));
}

template<char... Chars>
struct Str {
    static constexpr std::size_t size = sizeof...(Chars);
    static constexpr char value[] = {Chars..., 0};

    operator std::string_view() const {
        return {value, size};
    }
};

template<char... CharsLeft, char... CharsRight>
Str<CharsLeft..., CharsRight...> operator+(Str<CharsLeft...>, Str<CharsRight...>) {
    return {};
}

class univ {
public:
    constexpr univ(auto&&...) {}

    template<class T>
    constexpr operator T() {
        return {};
    }

    static void show_type(auto& stream) {
        stream.write("!");
    }

    friend void show(auto& stream, univ) {
        stream.write("!");
    }
};

template<class... Ts>
struct specific_type;

template<class... Ts>
    requires (!std::same_as<std::remove_cvref_t<Ts>, Ts> || ...)
struct specific_type<Ts...> : public specific_type<std::remove_cvref_t<Ts>...> {};

template<class... Ts>
using specific_type_t = specific_type<Ts...>::type;

template<class... Ts>
constexpr specific_type_t<Ts...> make_specific(Ts&&... values) {
    return specific_type<Ts...>::make(std::forward<Ts>(values)...);
}

template<class T>
struct specific_type<T> {
    using type = T;

    static type make(T&& value) {
        return value;
    }
};

template<class T>
struct specific_type<T, T> {
    using type = T;

    static type make(T&& value, T&&) {
        return value;
    }
};

template<class T1, class T2, class T3, class... Ts>
struct specific_type<T1, T2, T3, Ts...> : public specific_type<specific_type_t<T1, T2>, T3, Ts...> {};

auto operator>>(auto&& arg, auto func) requires std::invocable<decltype(func), decltype(arg)> {
    return func(std::forward<decltype(arg)>(arg));
}

template<template<class...> class T, class... Args>
struct unique_pack;

template<template<class...> class T>
struct unique_pack<T> {
    using type = T<>;
};

template<template<class...> class T, class Arg, class... Args>
    requires (std::same_as<Arg, Args> || ...)
struct unique_pack<T, Arg, Args...> {
    using type = unique_pack<T, Args...>::type;
};

template<template<class...> class T, class Arg, class... Args>
    requires (!(std::same_as<Arg, Args> || ...))
struct unique_pack<T, Arg, Args...> {
    template<class... TArgs>
    using U = T<Arg, TArgs...>;

    using type = unique_pack<U, Args...>::type;
};

template<template<class...> class T, class... Args>
using unique_pack_t = unique_pack<T, Args...>::type;

template<class... Funcs>
struct overload : public Funcs... {
    using Funcs::operator()...;

    overload(std::convertible_to<Funcs> auto&&... funcs) :
        Funcs(std::forward<Funcs>(funcs))...
    {}
};

template<class... Funcs>
overload(Funcs...) -> overload<Funcs...>;

struct unsafe {};

}
