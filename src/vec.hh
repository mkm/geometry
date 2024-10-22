#pragma once

#include <cstdint>
#include <utility>
#include <algorithm>
#include <numeric>
#include <type_traits>
#include <concepts>
#include <optional>

#include "static.hh"
#include "sub_array.hh"
#include "format.hh"

namespace geometry {

template<class T>
constexpr bool is_vec_tag = false;

template<class T>
concept is_vec = is_vec_tag<T>;

template<class Scalar, std::size_t Dim>
class broadcast {
public:
    static constexpr std::size_t dim = Dim;

private:
    Scalar _value;

public:
    broadcast(std::convertible_to<Scalar> auto&& value, index<Dim> = {}) :
        _value{value}
    {}

    Scalar const& get() const {
        return _value;
    }

    template<std::size_t I>
    Scalar const& get(index<I>) const {
        return get();
    }

    template<std::size_t I>
    Scalar const& operator[](index<I>) const {
        return get();
    }

    static void show_type(auto& stream) {
        stream.write("(");
        show(stream, proxy<Scalar>{});
        stream.write("… ×");
        show(stream, dim);
        stream.write(")");
    }

    friend void show(auto& stream, broadcast const& v) {
        stream.write("(");
        show(stream, v._value);
        stream.write("… ×");
        show(stream, dim);
        stream.write(")");
    }
};

template<class Scalar, std::size_t Dim>
broadcast(Scalar, index<Dim>) -> broadcast<Scalar, Dim>;

template<class Scalar, std::size_t Dim>
constexpr bool is_vec_tag<broadcast<Scalar, Dim>> = true;

template<class Scalar, std::size_t Dim>
constexpr bool is_nonzero_tag<broadcast<Scalar, Dim>> = is_nonzero<Scalar> && Dim > 0;

template<class ScalarLeft, class ScalarRight, std::size_t Dim>
auto operator+(broadcast<ScalarLeft, Dim> const& left, broadcast<ScalarRight, Dim> const& right)
    -> broadcast<decltype(left.get() + right.get()), Dim>
{
    return left.get() + right.get();
}

template<class ScalarLeft, class ScalarRight, std::size_t Dim>
auto operator*(broadcast<ScalarLeft, Dim> const& left, broadcast<ScalarRight, Dim> const& right)
    -> broadcast<decltype(left.get() * right.get()), Dim>
{
    return left.get() * right.get();
}

template<class... Scalars>
class vec {
    template<class...>
    friend class vec;

public:
    static constexpr std::size_t dim = sizeof...(Scalars);

private:
    std::tuple<Scalars...> _coords;

    template<std::size_t... Ix>
    static vec from_func_iota(auto func, std::index_sequence<Ix...>) {
        return {func(index<Ix>{})...};
    }

    template<std::size_t... Ix>
    auto with_coords_iota(auto func, std::index_sequence<Ix...>) const {
        return func(get(index<Ix>{})...);
    }

    template<std::size_t... Ix>
    auto with_coords_iota(auto func, std::index_sequence<Ix...>) && {
        return func(std::move(get(index<Ix>{}))...);
    }

public:
    vec() = default;

    vec(std::tuple<Scalars...> coords) :
        _coords{std::move(coords)}
    {}

    template<std::convertible_to<Scalars>... Ts>
    vec(Ts&&... coords) :
        _coords{std::forward<decltype(coords)>(coords)...}
    {}

    template<class... OtherScalars>
    vec(vec<OtherScalars...> const& that) :
        _coords{that._coords}
    {}

    static vec from_func(auto func) {
        return from_func_iota(func, std::make_index_sequence<dim>{});
    }

    template<std::size_t I>
    auto& get(index<I>) {
        return std::get<I>(_coords);
    }

    template<std::size_t I>
    auto const& get(index<I>) const {
        return std::get<I>(_coords);
    }

    template<std::size_t I>
    auto const& operator[](index<I> i) const {
        return get(i);
    }

    std::common_type_t<Scalars...> const& operator[](std::size_t i) const {
        return _coords[i];
    }

    auto with_coords(auto func) const {
        return with_coords_iota(func, std::make_index_sequence<dim>{});
    }

    auto with_coords(auto func) && {
        return std::move(*this).with_coords_iota(std::move(func), std::make_index_sequence<dim>{});
    }

    template<class... ScalarsLeft, class... ScalarsRight>
    friend std::strong_ordering operator<=>(vec<ScalarsLeft...> const&, vec<ScalarsRight...> const&);

    static void show_type(auto& stream) {
        stream.write("(");
        show_many(stream, " ", proxy<Scalars>{}...);
        stream.write(")");
    }

    friend void show(auto& stream, vec const& vec) {
        stream.write("(");
        show_tuple(stream, " ", vec._coords);
        stream.write(")");
    }
};

template<class... Scalars>
vec(Scalars... coords) -> vec<Scalars...>;

template<class... Scalars>
constexpr bool is_vec_tag<vec<Scalars...>> = true;

template<class... Scalars>
constexpr bool is_nonzero_tag<vec<Scalars...>> = (is_nonzero<Scalars> || ...);

template<std::size_t Dim, std::size_t I>
using basis_vec = decltype(
    []<std::size_t... Ix>(std::index_sequence<Ix...>)
        -> vec<std::conditional_t<I == Ix, one, zero>...>
    {}(std::make_index_sequence<Dim>{}));

template<std::size_t Dim, std::size_t I>
constexpr basis_vec<Dim, I> basis(index<I>) {
    static_assert(I < Dim, "basis index out of bounds");
    return {};
}

template<std::size_t Dim>
using basis_set = decltype(
    []<std::size_t... Ix>(std::index_sequence<Ix...>)
        -> vec<basis_vec<Dim, Ix>...>
    {}(std::make_index_sequence<Dim>{}));

template<std::size_t Dim>
constexpr basis_set<Dim> basis() {
    return {};
};

template<std::size_t Dim>
constexpr auto vec_from_func(auto func) {
    return std::make_index_sequence<Dim>{} >> [&]<std::size_t... Ix>(std::index_sequence<Ix...>) {
        return vec{func(index<Ix>{})...};
    };
}

template<class... ScalarsLeft, class... ScalarsRight>
vec<ScalarsLeft..., ScalarsRight...> cat(vec<ScalarsLeft...> left, vec<ScalarsRight...> right) {
    return std::move(left).with_coords([right = std::move(right)](ScalarsLeft&&... lefts) mutable {
        return std::move(right).with_coords([...lefts = std::move(lefts)](ScalarsRight&&... rights) mutable {
            return vec{std::move(lefts)..., std::move(rights)...};
        });
    });
}

template<class... ScalarsLeft, class... ScalarsRight>
std::strong_ordering operator<=>(vec<ScalarsLeft...> const& left, vec<ScalarsRight...> const& right) {
    return left._coords <=> right._coords;
}

template<class... ScalarsLeft, class... ScalarsRight>
bool operator==(vec<ScalarsLeft...> const& left, vec<ScalarsRight...> const& right) {
    return (left <=> right) == 0;
}

template<class... ScalarsLeft, class... ScalarsRight>
    requires (!std::same_as<ScalarsLeft, ScalarsRight> || ...)
struct specific_type<vec<ScalarsLeft...>, vec<ScalarsRight...>> {
    using type = vec<specific_type_t<ScalarsLeft, ScalarsRight>...>;

    static type make(vec<ScalarsLeft...> const& left, vec<ScalarsRight...> const& right) {
        return type::from_func([&](auto i) {
            return make_specific(left[i], right[i]);
        });
    }
};

template<class... ScalarsLeft, class... ScalarsRight>
auto operator+(vec<ScalarsLeft...> const& left, vec<ScalarsRight...> const& right)
    requires (sizeof...(ScalarsLeft) == sizeof...(ScalarsRight))
{
    return vec_from_func<sizeof...(ScalarsLeft)>([&](auto i) {
        return left[i] + right[i];
    });
}

template<class... ScalarsLeft, class... ScalarsRight>
auto operator*(vec<ScalarsLeft...> const& left, vec<ScalarsRight...> const& right)
    requires (sizeof...(ScalarsLeft) == sizeof...(ScalarsRight))
{
    return vec_from_func<sizeof...(ScalarsLeft)>([&](auto i) {
        return left[i] * right[i];
    });
}

template<class... ScalarsLeft, class ScalarRight, std::size_t DimRight>
auto operator*(vec<ScalarsLeft...> const& left, broadcast<ScalarRight, DimRight> const& right) {
    return vec_from_func<DimRight>([&](auto i) {
        return left[i] * right[i];
    });
}

template<class ScalarLeft, std::size_t DimLeft, class... ScalarsRight>
auto operator*(broadcast<ScalarLeft, DimLeft> const& left, vec<ScalarsRight...> const& right) {
    return vec_from_func<DimLeft>([&](auto i) {
        return left[i] * right[i];
    });
}

template<class... Scalars>
auto vsum(vec<Scalars...> const& v) {
    return v.with_coords([](auto const&... c) {
        return geometry::sum(c...);
    });
}

template<class... ScalarsLeft, class... ScalarsRight>
auto dot(vec<ScalarsLeft...> const& left, vec<ScalarsRight...> const& right) {
    return vsum(left * right);
}

template<class... ScalarsLeft, class... ScalarsRight>
bool collinear(vec<ScalarsLeft...> const& left, vec<ScalarsRight...> const& right) {
    /*
    // static_fold?
    auto test = overload{
        [](std::index_sequence<>) {
            return true;
        },
        [&]<std::size_t I, std::size_t... Ix>(std::index_sequence<I, Ix...>) {
            bool lzero = left[index<I>()] == 0_const;
            bool rzero = right[index<I>()] == 0_const;
            if (lzero && rzero) {
                return test(std::index_sequence<Ix...>{});
            }
        }
    };

    return test(std::make_index_sequence<left.dim>{});
    */
}

template<class Base, class Vec>
    requires (is_nonzero<Base> || is_nonzero<Vec>)
class proj {
private:
    [[no_unique_address]] Base _base;
    [[no_unique_address]] Vec _contents;

public:
    proj(Base base, Vec contents) :
        _base{base},
        _contents{contents}
    {}

    auto repr() const
        requires is_nonzero<Base>
    {
        auto g = _contents.with_coords([&](auto&... coords) {
            return gcd(_base, coords...);
        });

        return _contents.with_coords([&](auto&... coords) {
            return vec{_base / g, (coords / g)...};
        });
    }

    template<class BaseLeft, class VecLeft, class BaseRight, class VecRight>
    friend std::strong_ordering operator<=>(proj<BaseLeft, VecLeft> const&, proj<BaseRight, VecRight> const&);

    template<class BaseLeft, class VecLeft, class BaseRight, class VecRight>
        requires (is_nonzero<BaseLeft> && is_nonzero<BaseRight>)
    friend auto operator+(proj<BaseLeft, VecLeft> const&, proj<BaseRight, VecRight> const&);

    static void show_type(auto& stream) {
        show(stream, proxy<Vec>{});
        stream.write("/");
        show(stream, proxy<Base>{});
    }

    friend void show(auto& stream, proj const& value) {
        show(stream, value._contents);
        stream.write("/");
        show(stream, value._base);
    }
};

template<class Base, class Vec>
constexpr bool is_nonzero_tag<proj<Base, Vec>> = is_nonzero<Vec>;

template<class Base, class... Scalars>
using point = proj<one, vec<Scalars...>>;

template<class... Scalars>
    requires is_nonzero<vec<Scalars...>>
using dir = proj<zero, vec<Scalars...>>;

template<class BaseLeft, class VecLeft, class BaseRight, class VecRight>
std::strong_ordering operator<=>(proj<BaseLeft, VecLeft> const& left, proj<BaseRight, VecRight> const& right) {
    if (left._base == 0_const) {
        if (right._base == 0_const) {
            return left._contents <=> right._contents;
        } else {
            return std::strong_ordering::greater;
        }
    } else {
        if (right._base == 0_const) {
            return std::strong_ordering::less;
        } else {
            return broadcast{right._base, index<VecLeft::dim>{}} * left._contents <=> broadcast{left._base, index<VecRight::dim>{}} * right._contents;
        }
    }
}

template<class BaseLeft, class VecLeft, class BaseRight, class VecRight>
    requires (is_nonzero<BaseLeft> && is_nonzero<BaseRight>)
    // could be: requires (is_nonzero<BaseLeft> || is_nonzero<BaseRight>)
auto operator+(proj<BaseLeft, VecLeft> const& left, proj<BaseRight, VecRight> const& right) {
    return proj{
        left._base * right._base,
        broadcast{right._base, index<VecLeft::dim>{}} * left._contents + right._contents * broadcast{left._base, index<VecRight::dim>{}}
    };
}

template<class... Scalars>
using Point = vec<one, Scalars...>;

template<class... Scalars>
Point<std::remove_cvref_t<Scalars>...> make_point(Scalars&&... coords) {
    return vec{1_const, std::forward<Scalars>(coords)...};
}

template<class... Scalars>
dir<std::remove_cvref_t<Scalars>...> make_dir(Scalars&&... coords) {
    return proj{vec{0_const, std::forward<Scalars>(coords)...}};
}

template<std::size_t Grade, class Scalar, class Name, class Ix, class Vec>
class WedgeBase;

template<class Scalar, class _Name, class Ix, class Vec>
class WedgeBase<0, Scalar, _Name, Ix, Vec> {
protected:
    using Name = std::conditional_t<std::same_as<_Name, Str<>>, Str<'*'>, _Name>;

    [[no_unique_address]] Scalar value;

    WedgeBase() = default;

    WedgeBase(Scalar value) :
        value{std::move(value)}
    {}

    std::tuple<std::tuple<Name, Scalar>> coords() const {
        return {{{}, value}};
    }

    Scalar pseudo_scalar() const {
        return value;
    }
};

template<std::size_t Grade, class Scalar, class Name, std::size_t... Ix, class Vec>
    requires (sizeof...(Ix) < Grade)
class WedgeBase<Grade, Scalar, Name, std::index_sequence<Ix...>, Vec> {
protected:
    std::tuple<> coords() const {
        return {};
    }
};

template<std::size_t Grade, class Scalar, char... Name, std::size_t I, std::size_t... Ix, class VScalar, class... VScalars>
    requires (Grade > 0 && sizeof...(Ix) + 1 >= Grade)
class WedgeBase<Grade, Scalar, Str<Name...>, std::index_sequence<I, Ix...>, vec<VScalar, VScalars...>> :
    private WedgeBase<Grade - 1, Mul<Scalar, VScalar>, Str<Name..., I + 'a'>, std::index_sequence<Ix...>, vec<VScalars...>>,
    private WedgeBase<Grade, Scalar, Str<Name...>, std::index_sequence<Ix...>, vec<VScalars...>>
{
private:
    using Base0 = WedgeBase<Grade - 1, Mul<Scalar, VScalar>, Str<Name..., I + 'a'>, std::index_sequence<Ix...>, vec<VScalars...>>;
    using Base1 = WedgeBase<Grade, Scalar, Str<Name...>, std::index_sequence<Ix...>, vec<VScalars...>>;

protected:
    WedgeBase() = default;

    WedgeBase(VScalar coord, VScalars... coords) requires (Grade == 1) :
        Base0{coord},
        Base1{coords...}
    {}

    auto coords() const {
        return std::tuple_cat(Base0::coords(), Base1::coords());
    }

    auto pseudo_scalar() const requires (Grade == sizeof...(Ix) + 1) {
        return Base0::pseudo_scalar();
    }
};

template<class Vec, std::size_t Grade>
class Wedge : private WedgeBase<Grade, one, Str<>, std::make_index_sequence<Vec::dim>, Vec> {
private:
    using Base = WedgeBase<Grade, one, Str<>, std::make_index_sequence<Vec::dim>, Vec>;

    template<std::size_t... Ix>
    Wedge(Vec v, std::index_sequence<Ix...>) :
        Base{v[index<Ix>()]...}
    {}

public:
    Wedge() = default;

    Wedge(Vec v) requires (Grade == 1) :
        Wedge{v, std::make_index_sequence<Vec::dim>{}}
    {}

    auto scalar() const requires (Grade == 0) {
        return Base::value;
    }

    auto pseudo_scalar() const requires (Grade == Vec::dim) {
        return Base::pseudo_scalar();
    }

    template<std::size_t GradeLeft, std::size_t GradeRight>
    friend Wedge<Vec, GradeLeft + GradeRight> wedge(Wedge<Vec, GradeLeft> const&, Wedge<Vec, GradeRight> const&);

    friend void show(auto& stream, Wedge const& wedge) {
        stream.write("<");
        std::apply([&stream](auto const& coord, auto const&... coords) {
                stream.write(std::get<0>(coord));
                stream.write(":");
                show(stream, std::get<1>(coord));
                ((stream.write(" "),
                  stream.write(std::get<0>(coords)),
                  stream.write(":"),
                  show(stream, std::get<1>(coords))), ...);
            }, wedge.coords());
        stream.write(">");
    }
};

template<class Vec>
Wedge(Vec) -> Wedge<Vec, 1>;

template<std::size_t Dim>
struct det_factor {
    std::array<std::size_t, Dim> summands;
    bool negate{false};
};

template<std::size_t Dim>
void show(auto& stream, det_factor<Dim> const& factor) {
    stream.write(factor.negate ? "-" : "+");
    show(stream, factor.summands);
}

template<std::size_t Dim>
constexpr std::array<det_factor<Dim>, factorial(Dim)> det_factors() {
    std::array<det_factor<Dim>, factorial(Dim)> result;
    std::array<std::size_t, Dim> summands;
    bool negate{false};
    std::iota(summands.begin(), summands.end(), 0);
    auto output = result.begin();

    if constexpr (Dim == 0) {
        output->summands = summands;
        output->negate = negate;
    } else {
        while (true) {
            output->summands = summands;
            output->negate = negate;
            ++output;

            auto rev_begin = summands.end() - 1;
            while (rev_begin > summands.begin()) {
                if (rev_begin[-1] < rev_begin[0]) {
                    break;
                }
                --rev_begin;
            }
            if (rev_begin == summands.begin()) {
                break;
            }
            auto rev_end = rev_begin;
            --rev_begin;
            for (auto rev_end_test = rev_end; rev_end_test != summands.end(); ++rev_end_test) {
                if (*rev_begin < *rev_end_test) {
                    rev_end = rev_end_test;
                }
            }

            std::iter_swap(rev_begin, rev_end);
            std::reverse(rev_begin + 1, summands.end());
            std::size_t swaps = 1 + (summands.end() - rev_begin - 1) / 2;
            // polarities seem to be + - - + + - - + …
            // if polarities are this regular, can std::next_permutation be used?
            if (swaps % 2 == 1) {
                negate = !negate;
            }
        }
    }

    return result;
}

template<std::size_t Dim, det_factor<Dim> factor, class... Vecs, std::size_t... Ix>
constexpr auto det_prod(std::tuple<Vecs const&...> vecs, std::index_sequence<Ix...>) {
    if constexpr (factor.negate) {
        return -prod(std::get<Ix>(vecs)[index<factor.summands[Ix]>()]...);
    } else {
        return prod(std::get<Ix>(vecs)[index<factor.summands[Ix]>()]...);
    }
}

template<std::size_t Dim, std::array<det_factor<Dim>, factorial(Dim)> factors, class... Vecs, std::size_t... Ix>
constexpr auto det_sum(std::tuple<Vecs const&...> vecs, std::index_sequence<Ix...>) {
    return sum(det_prod<Dim, factors[Ix]>(vecs, std::make_index_sequence<Dim>{})...);
}

constexpr auto det(auto const&... vecs) {
    constexpr std::size_t Dim = sizeof...(vecs);
    constexpr std::array<det_factor<Dim>, factorial(Dim)> factors = det_factors<Dim>();
    return det_sum<Dim, factors>(std::tie(vecs...), std::make_index_sequence<factors.size()>{});
}

}

template<class... ScalarsLeft, class... ScalarsRight>
struct std::common_type<geometry::vec<ScalarsLeft...>, geometry::vec<ScalarsRight...>> {
    using type = geometry::vec<std::common_type_t<ScalarsLeft, ScalarsRight>...>;
};
