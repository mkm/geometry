#pragma once

#include <array>

namespace geometry {

template<class... Points>
class plane_through {
private:
    std::tuple<Points...> _points;

public:
    template<std::convertible_to<Points>... Ts>
    plane_through(Ts&&... points) :
        _points{std::forward<Ts>(points)...}
    {}

    constexpr std::tuple<Points...> const& points() const {
        return _points;
    }

    auto with_points(auto&& func) const {
        return std::apply(std::forward<decltype(func)>(func), _points);
    }

    constexpr auto normal() const {
        static constexpr std::size_t dim = sizeof...(Points) + 1;
        return vec_from_func<dim>(
            [this]<std::size_t I>(index<I> i) {
                return this->with_points([i](auto const&... points) {
                    return det(basis<dim>(i), points...);
                });
            });
    }

    bool contains(auto const& point) const {
        return dot(normal(), point) == 0_const;
    }

    static void show_type(auto& stream) {
        stream.write("(");
        show_many(stream, " ∧ ", proxy<Points>{}...);
        stream.write(")");
    }

    friend void show(auto& stream, plane_through const& plane) {
        stream.write("(");
        show_tuple(stream, " ∧ ", plane._points);
        stream.write(")");
    }
};

template<class... Points>
plane_through(Points... points) -> plane_through<Points...>;

template<class T>
concept is_plane_through =
    decltype(overload{
        []<class... Points>(plane_through<Points...> const&) { return std::true_type{}; },
        [](auto const&) { return std::false_type{}; }
    }(std::declval<T>()))::value;

template<class... Normals>
class ImplicitPlane {
private:
    std::tuple<Normals...> _normals;

public:
    ImplicitPlane(std::convertible_to<Normals> auto&&... normals) :
        _normals{normals...}
    {}

    template<class... Points>
    ImplicitPlane(plane_through<Points...> const& plane)
        requires (sizeof...(Normals) == 1 && ((sizeof...(Points) + 1 == Normals::dim) && ...)) :
        _normals{plane.normal()}
    {}

    auto with_normals(auto&& func) const {
        return std::apply(std::forward<decltype(func)>(func), _normals);
    }

    bool contains(auto const& point) const {
        return std::apply([&](auto const&... normals) {
            return ((dot(normals, point) == 0_const) && ...);
        }, _normals);
    }

    static void show_type(auto& stream) {
        stream.write("(");
        show_many(stream, " & ", proxy<Normals>{}...);
        stream.write(")");
    }

    friend void show(auto& stream, ImplicitPlane const& plane) {
        stream.write("(");
        show_tuple(stream, " & ", plane._normals);
        stream.write(")");
    }
};

template<class... Normals>
ImplicitPlane(Normals...) -> ImplicitPlane<Normals...>;

template<class... Points>
ImplicitPlane(plane_through<Points...> plane) -> ImplicitPlane<decltype(plane.normal())>;

template<class T>
concept IsImplicitPlane =
    decltype(overload{
        []<class... Points>(ImplicitPlane<Points...> const&) { return std::true_type{}; },
        [](auto const&) { return std::false_type{}; }
    }(std::declval<T>()))::value;

template<is_plane_through... Planes>
class PlaneIntersection {
private:
    std::tuple<Planes...> _planes;

public:
    template<std::convertible_to<Planes>... Ts>
    PlaneIntersection(Ts&&... planes) :
        _planes{std::forward<Ts>(planes)...}
    {}

    PlaneIntersection(std::tuple<Planes...> planes) :
        _planes(std::move(planes))
    {}

    std::tuple<Planes...> const& planes() const {
        return _planes;
    }

    static void show_type(auto& stream) {
        stream.write("(");
        show_many(stream, " ∩ ", proxy<Planes>{}...);
        stream.write(")");
    }

    friend void show(auto& stream, PlaneIntersection const& plane_int) {
        stream.write("(");
        show_tuple(stream, " ∩￼", plane_int._planes);
        stream.write(")");
    }
};

}
