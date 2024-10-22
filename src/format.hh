#pragma once

#include <cstdio>
#include <cinttypes>
#include <string>
#include <string_view>
#include <typeinfo>
#include <array>
#include <vector>
#include <tuple>
#include <cxxabi.h>

namespace geometry {

class FileStream {
private:
    FILE* _file;

public:
    FileStream() :
        FileStream{stdout}
    {}

    FileStream(FILE* file) :
        _file{file}
    {}

    void write(char c) {
        std::fputc(c, _file);
    }

    void write(std::string_view str) {
        std::fputs(str.data(), _file);
    }
};

void show_many(auto&) {}

void show_many(auto& stream, std::string_view sep, auto&& value, auto&&... values) {
    show(stream, value);
    ((stream.write(sep), show(stream, values)), ...);
}

template<class... Ts>
void show_tuple(auto& stream, std::string_view sep, std::tuple<Ts...> values) {
    std::apply([&stream, sep](auto&&... values) { show_many(stream, sep, std::forward<decltype(values)>(values)...); }, values);
}

void show_range(auto& stream, std::string_view sep, auto begin, auto end) {
    if (begin != end) {
        show(stream, *begin);
        ++begin;
        while (begin != end) {
            stream.write(sep);
            show(stream, *begin);
            ++begin;
        }
    }
}

void print(auto&&... values) {
    FileStream stream{};
    show_many(stream, "   ", std::forward<decltype(values)>(values)...);
    stream.write("\n");
}

void show(auto& stream, bool value) {
    stream.write(value ? "true" : "false");
}

void show(auto& stream, uint64_t value) {
    std::array<char, 24> str;
    std::sprintf(str.data(), "%" PRIu64, value);
    stream.write(str.data());
}

void show(auto& stream, int64_t value) {
    std::array<char, 24> str;
    std::sprintf(str.data(), "%" PRIi64, value);
    stream.write(str.data());
}

void show(auto& stream, std::string_view s) {
    stream.write("\"");
    stream.write(s);
    stream.write("\"");
}

void show(auto& stream, std::type_info const& info) {
    char* pretty_name = abi::__cxa_demangle(info.name(), nullptr, nullptr, nullptr);
    stream.write(pretty_name);
    std::free(pretty_name);
}

template<class T>
struct proxy {
    friend void show(auto& stream, proxy) {
        if constexpr (requires { T::show_type(stream); }) {
            T::show_type(stream);
        } else {
            show(stream, typeid(T));
        }
    }
};

template<class T>
struct ShowType {
    T value;
};

template<class T>
ShowType(T) -> ShowType<T>;

template<class T>
void show(auto& stream, ShowType<T> const& t) {
    show_many(stream, " : ", t.value, proxy<T>{});
}

void printt(auto&&... values) {
    print(ShowType{std::forward<decltype(values)>(values)}...);
}

void show(auto& stream, std::strong_ordering ord) {
    if (ord < 0) {
        stream.write("'<'");
    } else if (ord > 0) {
        stream.write("'>'");
    } else {
        stream.write("'='");
    }
}

template<class T, std::size_t N>
void show(auto& stream, std::array<T, N> const& value) {
    stream.write("[");
    show_range(stream, " ", value.begin(), value.end());
    stream.write("]");
}

template<class T>
void show(auto& stream, std::vector<T> const& value) {
    stream.write("[");
    show_range(stream, " ", value.begin(), value.end());
    stream.write("]");
}

template<class... Ts>
void show(auto& stream, std::tuple<Ts...> const& value) {
    stream.write("tuple(");
    show_tuple(stream, ", ", value);
    stream.write(")");
}

}
