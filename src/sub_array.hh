#pragma once

#include <cstdint>
#include <utility>

#include "format.hh"

namespace geometry {

template<class T, std::size_t MaxSize>
class sub_array {
    template<class, std::size_t>
    friend class sub_array;

public:
    using Size = decltype([]() {
            if constexpr (MaxSize < 0x100) {
                return static_cast<std::uint8_t>(0);
            } else if constexpr (MaxSize < 0x10000) {
                return static_cast<std::uint16_t>(0);
            } else if constexpr (MaxSize < 0x100000000ULL) {
                return static_cast<std::uint32_t>(0);
            } else {
                return static_cast<std::size_t>(0);
            }
        }());

private:
    Size _size;
    alignas(T) std::byte _data[MaxSize * sizeof(T)];

public:
    sub_array() :
        _size{0}
    {}

    sub_array(std::convertible_to<T> auto&&... values) requires (sizeof...(values) <= MaxSize) :
        _size{sizeof...(values)}
    {
        std::make_index_sequence<sizeof...(values)>{} >> [this, &values...]<std::size_t... Ix>(std::index_sequence<Ix...>) {
            (std::construct_at(data() + Ix, std::forward<decltype(values)>(values)), ...);
        };
    }

    template<std::convertible_to<T> U, std::size_t ThatMaxSize>
        requires (ThatMaxSize <= MaxSize)
    sub_array(sub_array<U, ThatMaxSize> const& that) :
        _size{that._size}
    {
        for (std::size_t i{0}; i < _size; ++i) {
            std::construct_at(data() + i, that[i]);
        }
    }

    sub_array(sub_array const& that) :
        _size{that._size}
    {
        for (std::size_t i{0}; i < _size; ++i) {
            std::construct_at(data() + i, that[i]);
        }
    }

    sub_array(sub_array&& that) :
        _size{that._size}
    {
        for (std::size_t i{0}; i < _size; ++i) {
            std::construct_at(data() + i, std::move(that[i]));
        }
        that._size = 0;
    }

    ~sub_array() {
        for (std::size_t i{0}; i < _size; ++i) {
            std::destroy_at(data() + i);
        }
    }

    sub_array& operator=(sub_array that) {
        swap(*this, that);
        return *this;
    }

    friend void swap(sub_array& left, sub_array& right) {
        using std::swap;
        swap(left._size, right._size);
        swap(left._data, right._data);
    }

    std::size_t size() const {
        return _size;
    }

    T* data() {
        return reinterpret_cast<T*>(_data);
    }

    T const* data() const {
        return reinterpret_cast<T const*>(_data);
    }

    T* begin() {
        return data();
    }

    T const* begin() const {
        return data();
    }

    T* end() {
        return data() + size();
    }

    T const* end() const {
        return data() + size();
    }

    T& operator[](std::size_t i) {
        return data()[i];
    }

    T const& operator[](std::size_t i) const {
        return data()[i];
    }

    void push_back(T value) {
        std::construct_at(end(), std::move(value));
        ++_size;
    }

    void emplace_back(auto&&... args) {
        std::construct_at(end(), std::forward<decltype(args)>(args)...);
        ++_size;
    }

    template<class Iter>
    void insert_back(Iter begin, Iter end) {
        while (begin != end) {
            push_back(*begin);
            ++begin;
        }
    }

    void erase(T* first, T* last) {
        auto it = first;
        while (it != last) {
            std::destroy_at(it);
            ++it;
        }
        auto to = first;
        auto from = last;
        last = end();
        while (from != last) {
            std::construct_at(to, std::move(*from));
            ++from;
            ++to;
        }
        _size = to - data();
    }

    static void show_type(auto& stream) {
        stream.write("[");
        show(stream, proxy<T>{});
        stream.write("; 0…");
        show(stream, MaxSize);
        stream.write("]");
    }

    friend void show(auto& stream, sub_array const& value) {
        stream.write("[");
        show_range(stream, " ", value.begin(), value.end());
        stream.write("]");
    }
};

}
