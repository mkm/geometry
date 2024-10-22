#pragma once

#include <optional>
#include <utility>

#include "vec.hh"
#include "plane.hh"
#include "set.hh"

namespace geometry {

template<class T, class U>
struct intersection_impl {};
// generic catch-all intersection?

template<class IntersectionImpl>
struct flipped_intersection {
    using type = IntersectionImpl::type;

    static type intersection(auto&& left, auto&& right) {
        return IntersectionImpl::intersection(std::forward<decltype(right)>(right), std::forward<decltype(left)>(left));
    }
};

template<class T, class U>
using intersection_class =
    std::conditional_t<
        requires { typename intersection_impl<std::remove_cvref_t<T>, std::remove_cvref_t<U>>::type; },
        intersection_impl<std::remove_cvref_t<T>, std::remove_cvref_t<U>>,
        flipped_intersection<intersection_impl<std::remove_cvref_t<U>, std::remove_cvref_t<T>>>>;

template<class T, class U>
using intersection_type = intersection_class<T, U>::type;

template<class T>
T intersection(T&& value) {
    return value;
}

template<class T, class U>
intersection_type<T, U> intersection(T&& left, U&& right) {
    return intersection_class<T, U>::intersection(std::forward<T>(left), std::forward<U>(right));
}

template<class T1, class T2, class T3, class... Ts>
auto intersection(T1&& value1, T2&& value2, T3&& value3, Ts... values) {
    return intersection(intersection(std::forward<T1>(value1), std::forward<T2>(value2)), std::forward<T3>(value3), std::forward<Ts>(values)...);
}

template<class T, class U>
bool intersects(T&& left, U&& right) {
    return intersection(std::forward<T>(left), std::forward<U>(right));
}

template<class... ScalarsLeft, class... ScalarsRight>
struct intersection_impl<vec<ScalarsLeft...>, vec<ScalarsRight...>> {
    using type = set<vec<specific_type_t<ScalarsLeft, ScalarsRight>...>, 1>;

    static type intersection(vec<ScalarsLeft...> const& left, vec<ScalarsRight...> const& right) {
        if (left == right) {
            return {make_specific(left, right)};
        } else {
            return {};
        }
    }
};

template<class... Scalars, class... Points>
struct intersection_impl<Point<Scalars...>, plane_through<Points...>> {
    using type = set<Point<Scalars...>, 1>;

    static type intersection(Point<Scalars...> const& left, plane_through<Points...> const& plane) {
        return plane.with_points([&left](auto&... points) -> type {
            if (det(left, points...) == 0_const) {
                return {left};
            } else {
                return {};
            }
        });
    }
};

template<class... PointsLeft, class... PointsRight>
struct intersection_impl<plane_through<PointsLeft...>, plane_through<PointsRight...>> {
    using type = PlaneIntersection<plane_through<PointsLeft...>, plane_through<PointsRight...>>;

    static type intersection(plane_through<PointsLeft...> left, plane_through<PointsRight...> right) {
        return {std::move(left), std::move(right)};
    }
};

template<class Point, class... Normals>
    requires requires (Point p, Normals... ns) { (dot(p, ns), ...); }
struct intersection_impl<Point, ImplicitPlane<Normals...>> {
    using type = set<Point, 1>;

    static type intersection(Point const& left, ImplicitPlane<Normals...> const& right) {
        if (right.contains(left)) {
            return {left};
        } else {
            return {};
        }
    }
};

/*
template<class... Normals, class Point>
    requires requires (Point p, Normals... ns) { (dot(p, ns), ...); }
struct intersection_impl<ImplicitPlane<Normals...>, Point> {
    using type = set<Point, 1>;

    static type intersection(ImplicitPlane<Normals...> const& left, Point const& right) {
        if (left.contains(right)) {
            return {right};
        } else {
            return {};
        }
    }
};
*/

template<class... NormalsLeft, class... NormalsRight>
struct intersection_impl<ImplicitPlane<NormalsLeft...>, ImplicitPlane<NormalsRight...>> {
    using type = ImplicitPlane<NormalsLeft..., NormalsRight...>;

    static type intersection(ImplicitPlane<NormalsLeft...> const& left, ImplicitPlane<NormalsRight...> const& right) {
        return left.with_normals([&](auto const&... left_normals) {
            return right.with_normals([&](auto const&... right_normals) {
                return ImplicitPlane{left_normals..., right_normals...};
            });
        });
    }
};

template<class... PlanesLeft, class... PlanesRight>
struct intersection_impl<PlaneIntersection<PlanesLeft...>, PlaneIntersection<PlanesRight...>> {
    using type = PlaneIntersection<PlanesLeft..., PlanesRight...>;

    static type intersection(PlaneIntersection<PlanesLeft...> const& left, PlaneIntersection<PlanesRight...> const& right) {
    }
};

template<class T, std::size_t MaxSize, class U>
struct intersection_impl<set<T, MaxSize>, U> {
    using type = std::conditional_t<is_set<intersection_type<T, U>>, enlarge_set<intersection_type<T, U>, MaxSize>, set<intersection_type<T, U>, MaxSize>>;

    static type intersection(set<T, MaxSize> const& left, U const& right) {
        if constexpr (is_set<intersection_type<T, U>>) {
            typename type::storage result;
            for (auto& elem : left) {
                auto subelems = geometry::intersection(elem, right);
                result.insert_back(subelems.begin(), subelems.end());
            }
            return result;
        } else {
            sub_array<intersection_type<T, U>, MaxSize> result;
            for (auto& elem : left) {
                result.push_back(geometry::intersection(elem, right));
            }
            return {result};
        }
    }
};

/*
template<class T, class U, std::size_t MaxSize>
    requires (!is_set<T>)
struct intersection_impl<T, set<U, MaxSize>> {
    using type = std::conditional_t<is_set<intersection_type<T, U>>, enlarge_set<intersection_type<T, U>, MaxSize>, set<intersection_type<T, U>, MaxSize>>;

    static type intersection(T const& left, set<U, MaxSize> const& right) {
        if constexpr (is_set<intersection_type<T, U>>) {
            typename type::storage result;
            for (auto& elem : right) {
                auto subelems = geometry::intersection(left, elem);
                result.insert_back(subelems.begin(), subelems.end());
            }
            return result;
        } else {
            sub_array<intersection_type<T, U>, MaxSize> result;
            for (auto& elem : right) {
                result.push_back(::intersection(left, elem));
            }
            return {result};
        }
    }
};
*/

}
