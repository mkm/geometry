#pragma once

#include <cstdint>
#include <array>
#include <algorithm>

#include "static.hh"
#include "format.hh"
#include "oneof.hh"

namespace geometry {

struct overflow_error {};
struct zero_error {};
struct division_by_zero_error {};

constexpr std::uint64_t abs(std::int64_t n) {
    return n < 0 ? -n : n;
}

constexpr std::size_t log2(std::size_t n) {
    if (n == 0) {
        return 0;
    } else {
        std::size_t r = 0;
        n -= 1;
        while (n > 0) {
            r += 1;
            n >>= 1;
        }
        return r;
    }
}

constexpr std::int64_t gcd(std::int64_t m, std::int64_t n) {
    m = abs(m);
    n = abs(n);
    if (m < n) std::swap(m, n);
    while (n > 0) {
        std::tie(m, n) = std::tuple{n, m % n};
    }
    return m;
}

auto gcd(auto&& n) {
    return n;
}

auto gcd(auto&& n1, auto&& n2, auto&&... ns) requires (sizeof...(ns) > 0) {
    return gcd(gcd(std::forward<decltype(n1)>(n1), std::forward<decltype(n2)>(n2)), std::forward<decltype(ns)>(ns)...);
}

constexpr std::int64_t lcm(std::int64_t m, std::int64_t n) {
    return (m * n) / gcd(m ,n);
}

constexpr std::int64_t factorial(std::int64_t n) {
    std::int64_t r = 1;
    while (n > 1) {
        r *= n;
        --n;
    }
    return r;
}

template<class ScalarLeft, class ScalarRight>
using Mul = decltype(std::declval<ScalarLeft>() * std::declval<ScalarRight>());

template<std::int64_t Value>
struct constant {
    static constexpr std::int64_t value = Value;

    constexpr operator std::int64_t() const {
        return value;
    }

    static void show_type(auto& stream) {
        show(stream, value);
    }

    friend void show(auto& stream, constant n) {
        show(stream, value);
    }
};

using zero = constant<0>;
using one = constant<1>;

constexpr std::int64_t parse_const(std::same_as<char> auto... cs) {
    std::int64_t value = 0;
    for (auto c : {cs...}) {
        value = value * 10 + (c - '0');
    }
    return value;
}

template<char... Digits>
constexpr constant<parse_const(Digits...)> operator""_const() {
    return {};
}

template<std::int64_t ValueLeft, std::int64_t ValueRight>
constexpr std::strong_ordering operator<=>(constant<ValueLeft>, constant<ValueRight>) {
    return ValueLeft <=> ValueRight;
}

template<std::int64_t ValueLeft, std::int64_t ValueRight>
constexpr bool operator==(constant<ValueLeft>, constant<ValueRight>) {
    return ValueLeft == ValueRight;
}

inline constexpr zero operator+(zero, zero) {
    return {};
}

template<class Scalar>
Scalar constexpr operator+(zero, Scalar right) {
    return right;
}

template<class Scalar>
Scalar constexpr operator+(Scalar left, zero) {
    return left;
}

template<std::int64_t ValueLeft, std::int64_t ValueRight>
constant<ValueLeft + ValueRight> constexpr operator+(constant<ValueLeft>, constant<ValueRight>) {
    return {};
}

template<std::int64_t Value>
constant<-Value> constexpr operator-(constant<Value>) {
    return {};
}

template<std::int64_t ValueLeft, std::int64_t ValueRight>
constant<ValueLeft - ValueRight> constexpr operator-(constant<ValueLeft>, constant<ValueRight>) {
    return {};
}

inline constexpr one operator*(one, one) {
    return {};
}

template<std::int64_t Value>
constexpr constant<Value> operator*(one, constant<Value>) {
    return {};
}

template<std::int64_t Value>
constexpr constant<Value> operator*(constant<Value>, one) {
    return {};
}

template<class Scalar>
constexpr Scalar operator*(one, Scalar right) {
    return right;
}

template<class Scalar>
constexpr Scalar operator*(Scalar left, one) {
    return left;
}

template<std::int64_t ValueLeft, std::int64_t ValueRight>
constexpr constant<ValueLeft * ValueRight> operator*(constant<ValueLeft>, constant<ValueRight>) {
    return {};
}

template<class T>
constexpr bool is_nonzero_tag = false;

template<class T>
concept is_nonzero = is_nonzero_tag<T>;

template<std::int64_t Value>
constexpr bool is_nonzero_tag<constant<Value>> = Value != 0;

template<class Num>
class nonzero {
private:
    Num _value;

public:
    nonzero(Num value) noexcept(is_nonzero<Num>) :
        _value{value}
    {
        if constexpr (!is_nonzero<Num>) {
            if (_value == 0_const) {
                throw zero_error{};
            }
        }
    }

    nonzero(unsafe, Num value) :
        _value{value}
    {}

    constexpr Num const& value() const {
        return _value;
    }

    static void show_type(auto& stream) {
        show(stream, proxy<Num>{});
        stream.write(" ≠0");
    }

    friend void show(auto& stream, nonzero const& n) {
        show(stream, n.value());
    }
};

template<class T>
constexpr bool is_nonzero_tag<nonzero<T>> = true;

template<class Num>
oneof<nonzero<Num>, zero> as_nonzero(Num value) {
    if (value == 0_const) {
        return zero{};
    } else {
        return nonzero{unsafe{}, value};
    }
}

template<class Num>
oneof<nonzero<Num>> as_nonzero(nonzero<Num> value) {
    return value;
}

template<class NumLeft, class NumRight>
auto operator<=>(nonzero<NumLeft> const& left, nonzero<NumRight> const& right) {
    return left.value() <=> right.value();
}

template<class NumLeft, class NumRight>
    requires (!is_nonzero<NumRight>)
auto operator<=>(nonzero<NumLeft> const& left, NumRight const& right) {
    return left.value() <=> right;
}

template<class NumLeft, class NumRight>
    requires (!is_nonzero<NumLeft>)
auto operator<=>(NumLeft const& left, nonzero<NumRight> const& right) {
    return left <=> right.value();
}

template<class NumLeft, class NumRight>
auto operator+(nonzero<NumLeft> const& left, nonzero<NumRight> const& right) {
    return left.value() + right.value();
}

template<class NumLeft, class NumRight>
auto operator*(nonzero<NumLeft> const& left, nonzero<NumRight> const& right) {
    return nonzero{left.value() * right.value()};
}

template<class NumLeft, class NumRight>
auto gcd(nonzero<NumLeft> const& left, NumRight const& right) {
    return nonzero{unsafe{}, gcd(left.value(), right)};
}

template<class NumLeft, class NumRight>
auto gcd(NumLeft const& left, nonzero<NumRight> const& right) {
    return nonzero{unsafe{}, gcd(left, right.value())};
}

template<class NumLeft, class NumRight>
auto gcd(nonzero<NumLeft> const& left, nonzero<NumRight> const& right) {
    return nonzero{unsafe{}, gcd(left.value(), right.value())};
}

template<>
class nonzero<zero> {
public:
    nonzero() = delete;
};

template<std::int64_t ValueLeft, std::int64_t ValueRight>
constexpr auto quotient(constant<ValueLeft>, constant<ValueRight>)
    -> std::tuple<constant<ValueLeft / ValueRight>, constant<ValueLeft % ValueRight>>
{
    static_assert(ValueRight != 0, "division by zero");
    return {};
}

// specialise division by 1

template<std::int64_t ValueLeft, std::int64_t ValueRight>
constexpr auto operator/(constant<ValueLeft>, constant<ValueRight>)
    -> constant<ValueLeft / ValueRight>
{
    static_assert(ValueRight != 0, "division by zero");
    return {};
}

constexpr one gcd(one, one) {
    return {};
}

template<std::int64_t ValueLeft>
constexpr one gcd(constant<ValueLeft>, one) {
    return {};
}

template<class NumLeft>
constexpr one gcd(NumLeft const&, one) {
    return {};
}

template<std::int64_t ValueRight>
constexpr one gcd(one, constant<ValueRight>) {
    return {};
}

template<class NumRight>
constexpr one gcd(one, NumRight const&) {
    return {};
}

template<std::int64_t ValueLeft, std::int64_t ValueRight>
constexpr constant<gcd(ValueLeft, ValueRight)> gcd(constant<ValueLeft>, constant<ValueRight>) {
    return {};
}

template<std::int64_t ValueLeft, std::int64_t ValueRight>
constexpr constant<lcm(ValueLeft, ValueRight)> lcm(constant<ValueLeft>, constant<ValueRight>) {
    return {};
}

template<std::int64_t Value>
constexpr constant<factorial(Value)> factorial(constant<Value>) {
    return {};
}

template<std::int64_t Value>
constexpr constant<Value> reduce(constant<Value>) {
    return {};
}

template<std::size_t Bits>
class Natural {
public:
    static constexpr std::size_t limb_count = (Bits + 63) / 64;

private:
    std::array<std::uint64_t, limb_count> _limbs;

public:
    Natural(uint64_t value) :
        _limbs{{value}}
    {
        if (Bits < 64 && value >= (1ULL << Bits)) {
            throw overflow_error{};
        }
    }

    friend void show(auto& stream, Natural const& n) {
        show(stream, n._limbs[0]);
    }
};

inline constexpr std::size_t add_bits(std::size_t bits_left, std::size_t bits_right) {
    return std::max(bits_left, bits_right) + 1;
}

template<std::size_t N>
struct sum_strategy {
    static constexpr std::size_t size = 2 * N;

    struct Node {
        std::size_t bits;
        std::size_t left;
        std::size_t right;
        std::size_t index;
    };

    std::array<Node, size> nodes;
    std::size_t root;

    constexpr std::size_t bits() const {
        return nodes[root].bits;
    }

    constexpr bool is_leaf() const {
        return nodes[root].index < size;
    }

    constexpr sum_strategy left() const {
        return sum_strategy{nodes, nodes[root].left};
    }

    constexpr sum_strategy right() const {
        return sum_strategy{nodes, nodes[root].right};
    }

    constexpr std::size_t index() const {
        return nodes[root].index;
    }
};

template<std::size_t N>
void show(auto& stream, sum_strategy<N> strat) {
    if (strat.is_leaf()) {
        stream.write(strat.index() + 'A');
        stream.write("i");
        show(stream, strat.bits());
    } else {
        stream.write("i");
        show(stream, strat.bits());
        stream.write("(");
        show(stream, strat.left());
        stream.write(" + ");
        show(stream, strat.right());
        stream.write(")");
    }
}

constexpr auto make_sum_strategy(std::convertible_to<std::size_t> auto... bits) -> sum_strategy<sizeof...(bits)> {
    using Node = sum_strategy<sizeof...(bits)>::Node;

    sum_strategy<sizeof...(bits)> strat{{{Node{bits}...}}};
    auto alloc = &strat.nodes[sizeof...(bits)];

    std::array<std::size_t, sizeof...(bits)> heap;
    for (std::size_t i = 0; i < heap.size(); ++i) {
        strat.nodes[i].index = i;
        heap[i] = i;
    }
    auto begin = heap.begin();
    auto end = heap.end();
    auto compare = [&](std::size_t left, std::size_t right) { return strat.nodes[left].bits > strat.nodes[right].bits; };

    std::make_heap(begin, end, compare);
    while (end - begin > 1) {
        std::pop_heap(begin, end, compare);
        --end;
        auto left = *end;
        std::pop_heap(begin, end, compare);
        --end;
        auto right = *end;
        *alloc = Node{add_bits(strat.nodes[left].bits, strat.nodes[right].bits), left, right, strat.size};
        *end = alloc - &strat.nodes[0];
        ++alloc;
        ++end;
        std::push_heap(begin, end, compare);
    }

    strat.root = alloc - 1 - &strat.nodes[0];
    return strat;
}

constexpr std::size_t const_bits(std::int64_t value) {
    return log2(abs(value)) + 1;
}

constexpr std::size_t sum_bits(std::convertible_to<std::size_t> auto... bits) {
    return make_sum_strategy(bits...).bits();
}

inline constexpr std::size_t mul_bits(std::size_t bits_left, std::size_t bits_right) {
    return bits_left + bits_right;
}

inline constexpr std::size_t gcd_bits(std::size_t bits_left, std::size_t bits_right) {
    return std::max(bits_left, bits_right);
}

template<std::size_t _Bits>
class integer {
    template<std::size_t>
    friend class integer;

public:
    static constexpr std::size_t Bits = _Bits;
    static constexpr std::size_t limb_count = (Bits + 63) / 64;

private:
    std::array<std::int64_t, limb_count> _limbs;

public:
    constexpr integer(int64_t value) :
        _limbs{{value}}
    {
        /*
        if (Bits < 64 && (value < -(1LL << (Bits - 1)) || value >= (1LL << (Bits - 1)))) {
            throw overflow_error{};
        }
        */
    }

    template<std::int64_t Value>
    constexpr integer(constant<Value>) :
        integer(Value)
    {}

    template<std::size_t OtherBits>
        requires (OtherBits < Bits)
    constexpr integer(integer<OtherBits> const& that) :
        _limbs{{that._limbs[0]}}
    {}

    template<std::size_t Bits, std::int64_t Value>
    friend constexpr bool operator==(integer<Bits> const&, constant<Value>);

    template<std::int64_t Value, std::size_t Bits>
    friend constexpr bool operator==(constant<Value>, integer<Bits> const&);

    template<std::size_t BitsLeft, std::size_t BitsRight>
    friend constexpr bool operator==(integer<BitsLeft> const&, integer<BitsRight> const&);

    template<std::size_t Bits, std::int64_t Value>
    friend constexpr std::strong_ordering operator<=>(integer<Bits> const&, constant<Value>);

    template<std::int64_t Value, std::size_t Bits>
    friend constexpr std::strong_ordering operator<=>(constant<Value>, integer<Bits> const&);

    template<std::size_t BitsLeft, std::size_t BitsRight>
    friend constexpr std::strong_ordering operator<=>(integer<BitsLeft> const&, integer<BitsRight> const&);

    template<std::size_t BitsLeft, std::size_t BitsRight>
    friend integer<add_bits(BitsLeft, BitsRight)> operator+(integer<BitsLeft> const&, integer<BitsRight> const&);

    template<std::size_t... Bits>
    friend integer<sum_bits(Bits...)> sum(integer<Bits> const&...);

    template<std::size_t Bits>
    friend integer<Bits> operator-(integer<Bits> const&);

    template<std::size_t BitsLeft, std::size_t BitsRight>
    friend integer<mul_bits(BitsLeft, BitsRight)> operator*(integer<BitsLeft> const&, integer<BitsRight> const&);

    template<std::size_t BitsLeft, std::size_t BitsRight>
    friend std::tuple<integer<BitsLeft>, integer<BitsRight>> quotient(integer<BitsLeft> const&, nonzero<integer<BitsRight>> const&);

    static void show_type(auto& stream) {
        stream.write("i");
        show(stream, Bits);
    }

    friend void show(auto& stream, integer const& n) {
        show(stream, n._limbs[0]);
    }
};

template<std::int64_t Value>
integer(constant<Value>) -> integer<const_bits(Value)>;

template<std::size_t Bits, std::int64_t Value>
constexpr bool operator==(integer<Bits> const& left, constant<Value> right) {
    static_assert(Bits <= 64, "not ready for multiple limbs");
    return left._limbs[0] == right.value;
}

template<std::int64_t Value, std::size_t Bits>
constexpr bool operator==(constant<Value> left, integer<Bits> const& right) {
    static_assert(Bits <= 64, "not ready for multiple limbs");
    return left.value == right._limbs[0];
}

template<std::size_t BitsLeft, std::size_t BitsRight>
constexpr bool operator==(integer<BitsLeft> const& left, integer<BitsRight> const& right) {
    static_assert(BitsLeft <= 64 && BitsRight <= 64, "not ready for multiple limbs");
    return left._limbs[0] == right._limbs[0];
}

template<std::size_t Bits, std::int64_t Value>
constexpr std::strong_ordering operator<=>(integer<Bits> const& left, constant<Value> right) {
    static_assert(Bits <= 64, "not ready for multiple limbs");
    return left._limbs[0] <=> right.value;
}

template<std::int64_t Value, std::size_t Bits>
constexpr std::strong_ordering operator<=>(constant<Value> left, integer<Bits> const& right) {
    static_assert(Bits <= 64, "not ready for multiple limbs");
    return left.value <=> right._limbs[0];
}

template<std::size_t BitsLeft, std::size_t BitsRight>
constexpr std::strong_ordering operator<=>(integer<BitsLeft> const& left, integer<BitsRight> const& right) {
    static_assert(BitsLeft <= 64 && BitsRight <= 64, "not ready for multiple limbs");
    return left._limbs[0] <=> right._limbs[0];
}

template<std::size_t BitsLeft, std::size_t BitsRight>
    requires (BitsLeft != BitsRight)
struct specific_type<integer<BitsLeft>, integer<BitsRight>> {
    using type = integer<std::min(BitsLeft, BitsRight)>;

    static type make(integer<BitsLeft> const& left, integer<BitsRight> const& right) {
        if constexpr (BitsLeft <= BitsRight) {
            return left;
        } else {
            return right;
        }
    }
};

template<std::size_t Bits, std::int64_t Value>
struct specific_type<integer<Bits>, constant<Value>> {
    using type = constant<Value>;

    static type make(integer<Bits> const&, constant<Value>) {
        return {};
    }
};

template<std::int64_t Value, std::size_t Bits>
struct specific_type<constant<Value>, integer<Bits>> {
    using type = constant<Value>;

    static type make(constant<Value>, integer<Bits> const&) {
        return {};
    }
};

template<std::size_t BitsLeft, std::size_t BitsRight>
integer<add_bits(BitsLeft, BitsRight)> operator+(integer<BitsLeft> const& left, integer<BitsRight> const& right) {
    static_assert(add_bits(BitsLeft, BitsRight) <= 64, "not ready for multiple limbs");
    return left._limbs[0] + right._limbs[0];
}

template<sum_strategy strat, std::size_t... Bits>
integer<strat.bits()> sum_impl(integer<Bits> const&... args) {
    if constexpr (strat.is_leaf()) {
        return select<strat.index()>(args...);
    } else {
        return sum_impl<strat.left()>(args...) + sum_impl<strat.right()>(args...);
    }
}

zero integer_sum() {
    return {};
}

template<std::size_t... Bits>
integer<sum_bits(Bits...)> integer_sum(integer<Bits> const&... args) requires (sizeof...(Bits) > 0) {
    return sum_impl<make_sum_strategy(Bits...)>(args...);
}

template<std::size_t Bits>
integer<Bits> operator-(integer<Bits> const& arg) {
    static_assert(Bits <= 64, "not ready for multiple limbs");
    return -arg._limbs[0];
}

template<std::size_t BitsLeft, std::size_t BitsRight>
integer<mul_bits(BitsLeft, BitsRight)> operator*(integer<BitsLeft> const& left, integer<BitsRight> const& right) {
    static_assert(mul_bits(BitsLeft, BitsRight) <= 64, "not ready for multiple limbs");
    return left._limbs[0] * right._limbs[0];
}

template<std::size_t BitsLeft, std::size_t BitsRight>
std::tuple<integer<BitsLeft>, integer<BitsRight>> quotient(integer<BitsLeft> const& left, nonzero<integer<BitsRight>> const& right) {
    static_assert(BitsLeft <= 64, "not ready for multiple limbs");
    static_assert(BitsRight <= 64, "not ready for multiple limbs");
    return {{left._limbs[0] / right.value()._limbs[0]}, {left._limbs[0] % right.value()._limbs[0]}};
}

template<std::size_t BitsLeft, std::size_t BitsRight>
integer<BitsLeft> operator/(integer<BitsLeft> const& left, nonzero<integer<BitsRight>> const& right) {
    return std::get<0>(quotient(left, right));
}

template<std::size_t BitsLeft, std::size_t BitsRight>
integer<BitsRight> operator%(integer<BitsLeft> const& left, nonzero<integer<BitsRight>> const& right) {
    return std::get<1>(quotient(left, right));
}

template<std::size_t Bits>
integer<Bits> abs(integer<Bits> const& value) {
    return value < 0_const ? -value : value;
}

template<std::size_t BitsLeft, std::size_t BitsRight>
integer<gcd_bits(BitsLeft, BitsRight)> gcd(integer<BitsLeft> left, integer<BitsRight> right) {
    left = abs(left);
    right = abs(right);
    if (left < right) std::swap(left, right);
    bool loop = true;
    while (loop) {
        as_nonzero(right).match(overload{
            [&](zero) {
                loop = false;
            },
            [&](is_nonzero auto&& right_nz) {
                std::tie(left, right) = std::tuple{right, left % right_nz};
            }
        });
    }
    return left;
}

template<std::size_t Bits>
auto operator*(one, integer<Bits> right) {
    return right;
}

template<std::int64_t Value, std::size_t Bits>
auto operator*(constant<Value> left, integer<Bits> const& right) {
    return integer{left} * right;
}

template<std::size_t Bits>
auto operator*(integer<Bits> left, one) {
    return left;
}

template<std::size_t Bits, std::int64_t Value>
auto operator*(integer<Bits> const& left, constant<Value> right) {
    return left * integer{right};
}

auto prod(auto&&... args) {
    return (std::forward<decltype(args)>(args) * ...);
}

template<std::size_t Bits>
constexpr integer<Bits> reduce(integer<Bits> n) {
    return n;
}

using i8 = integer<8>;
using i16 = integer<16>;
using i32 = integer<32>;
using i64 = integer<64>;

inline i8 operator""_i8(unsigned long long value) {
    return value;
}

inline i16 operator""_i16(unsigned long long value) {
    return value;
}

inline i32 operator""_i32(unsigned long long value) {
    return value;
}

inline i64 operator""_i64(unsigned long long value) {
    return value;
}

template<std::int64_t... Values>
class Enum {
public:
    static constexpr std::size_t count = sizeof...(Values);
    static constexpr std::array<std::int64_t, count> values{{Values...}};

private:
    // should be Natural<log2(count)>
    std::size_t _index;

    static constexpr std::optional<std::size_t> index_of(std::int64_t value) {
        for (std::size_t i = 0; i < count; ++i) {
            if (value == values[i]) {
                return i;
            }
        }

        return {};
    }

public:
    template<std::int64_t Value>
    constexpr Enum(constant<Value>) requires ((Value == Values) || ...) :
        _index(index<*index_of(Value)>::value)
    {}

    friend auto operator<=>(Enum, Enum) = default;

    friend void show(auto& stream, Enum value) {
        show(stream, values[value._index]);
    }
};

template<std::size_t Bits, std::int64_t Base>
class AffineInteger {
private:
    [[no_unique_address]] integer<Bits> _offset;
    [[no_unique_address]] constant<Base> _base;

public:
    AffineInteger(integer<Bits> offset, constant<Base>) :
        _offset{offset},
        _base{}
    {}

    AffineInteger(constant<Base>) :
        _offset{0},
        _base{}
    {}

    auto& offset() const {
        return _offset;
    }

    friend auto reduce(AffineInteger const& value) {
        if constexpr (Base == 0) {
            return value._offset;
        } else {
            return value;
        }
    }

    template<std::size_t BitsLeft, std::int64_t BaseLeft, std::size_t BitsRight, std::int64_t BaseRight>
    friend constexpr bool operator==(AffineInteger<BitsLeft, BaseLeft> const&, AffineInteger<BitsRight, BaseRight> const&);

    static void show_type(auto& stream) {
        stream.write("(");
        show(stream, proxy<decltype(_offset)>{});
        stream.write(" + ");
        show(stream, proxy<decltype(_base)>{});
        stream.write(")");
    }

    friend void show(auto& stream, AffineInteger const& value) {
        if constexpr (Base == 0) {
            show(stream, value._offset);
        } else {
            stream.write("(");
            show_many(stream, " + ", value._offset, value._base);
            stream.write(")");
        }
    }
};

template<std::int64_t Base>
AffineInteger(constant<Base>) -> AffineInteger<1, Base>;

template<std::size_t BitsLeft, std::int64_t BaseLeft, std::size_t BitsRight, std::int64_t BaseRight>
constexpr bool operator==(AffineInteger<BitsLeft, BaseLeft> const& left, AffineInteger<BitsRight, BaseRight> const& right) {
    if constexpr (BitsLeft < BitsRight) {
        return left._offset + integer{left._base - right._base} == right._offset;
    } else {
        return left._offset == right._offset + integer{right._base - left._base};
    }
}

template<std::size_t Bits, std::int64_t Base, std::int64_t Value>
constexpr bool operator==(AffineInteger<Bits, Base> const& left, constant<Value> right) {
    return left == AffineInteger{right};
}

template<std::int64_t Value, std::size_t Bits, std::int64_t Base>
constexpr bool operator==(constant<Value> right, AffineInteger<Bits, Base> const& left) {
    return AffineInteger{left} == right;
}

template<class... Nums>
class Sum;

template<std::size_t I, class... Nums>
struct SumReduce;

template<std::size_t I>
struct SumReduce<I> {
    static constexpr std::int64_t base = 0;
    using Integers = std::index_sequence<>;
};

template<std::size_t I, std::int64_t Value, class... Nums>
struct SumReduce<I, constant<Value>, Nums...> {
    using Tail = SumReduce<I + 1, Nums...>;

    static constexpr std::int64_t base = Value + Tail::base;
    using Integers = Tail::Integers;
};

template<std::size_t I, std::size_t Bits, class... Nums>
struct SumReduce<I, integer<Bits>, Nums...> {
    using Tail = SumReduce<I + 1, Nums...>;

    static constexpr std::int64_t base = Tail::base;
    using Integers = CatSeq<std::index_sequence<I>, typename Tail::Integers>;
};

template<std::size_t I, std::size_t Bits, std::int64_t Base, class... Nums>
struct SumReduce<I, AffineInteger<Bits, Base>, Nums...> {
    using Tail = SumReduce<I + 1, Nums...>;

    static constexpr std::int64_t base = Base + Tail::base;
    using Integers = CatSeq<std::index_sequence<I>, typename Tail::Integers>;
};

template<class... Nums>
class Sum {
private:
    template<class...>
    friend class Sum;

    std::tuple<Nums...> _summands;

    Sum(std::tuple<Nums...> summands) :
        _summands(std::move(summands))
    {}

    auto sub_reduce() const {
        return std::apply([](auto const&... summands) {
                return Sum<decltype(reduce(summands))...>{reduce(summands)...};
            }, _summands);
    }

    auto reduce_impl() const {
        using Red = SumReduce<0, Nums...>;
        auto get_integer = overload{
            []<std::size_t Bits>(integer<Bits> const& v) { return v; },
            []<std::size_t Bits, std::int64_t Base>(AffineInteger<Bits, Base> const& v) { return v.offset(); },
        };
        auto const_value = constant<Red::base>{};
        auto integer_value = typename Red::Integers() >> [&]<std::size_t... Ix>(std::index_sequence<Ix...>) {
            return integer_sum(get_integer(std::get<Ix>(_summands))...);
        };
        if constexpr (const_value == 0_const) {
            return integer_value;
        } else if constexpr (std::same_as<decltype(integer_value), zero>) {
            return const_value;
        } else {
            return AffineInteger{integer_value, const_value};
        }
    }

public:
    Sum(std::convertible_to<Nums> auto&&... summands) :
        _summands{summands...}
    {}

    template<class... NumsLeft, class... NumsRight>
    friend constexpr Sum<NumsLeft..., NumsRight...> operator+(Sum<NumsLeft...> const&, Sum<NumsRight...> const&);

    friend auto reduce(Sum const& sum) {
        return sum.sub_reduce().reduce_impl();
    }

    template<class... NumsLeft, class... NumsRight>
    friend constexpr bool operator==(Sum<NumsLeft...> const&, Sum<NumsRight...> const&);

    friend void show(auto& stream, Sum const& sum) {
        stream.write("(");
        show_tuple(stream, " + ", sum._summands);
        stream.write(")");
    }
};

template<class... Nums>
Sum(Nums...) -> Sum<Nums...>;

template<class... NumsLeft, class... NumsRight>
constexpr Sum<NumsLeft..., NumsRight...> operator+(Sum<NumsLeft...> const& left, Sum<NumsRight...> const& right) {
    return std::tuple_cat(left._summands, right._summands);
}

template<class... NumsLeft, class... NumsRight>
constexpr bool operator==(Sum<NumsLeft...> const& left, Sum<NumsRight...> const& right) {
    return reduce(left) == reduce(right);
}

template<class... Nums>
constexpr auto sum(Nums&&... args) {
    return reduce(Sum{std::forward<Nums>(args)...});
}

}

template<std::int64_t ValueLeft, std::int64_t ValueRight>
struct std::common_type<geometry::constant<ValueLeft>, geometry::constant<ValueRight>> {
    using type = std::int64_t;
};

template<std::size_t BitsLeft, std::size_t BitsRight>
struct std::common_type<geometry::integer<BitsLeft>, geometry::integer<BitsRight>> {
    using type = geometry::integer<std::max(BitsLeft, BitsRight)>;
};
