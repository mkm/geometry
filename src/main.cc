#include "sub_array.hh"
#include "integer.hh"
#include "vec.hh"
#include "oneof.hh"
#include "set.hh"
#include "plane.hh"
#include "intersection.hh"

int main(int argc, char* argv[]) {
    using namespace geometry;

    auto p = make_point(0_i8, 0_i8);
    auto q = make_point(0_i8, 1_i8);
    auto r = make_point(2_i8, 0_i8);
    auto s = make_point(1_i8, 1_i8);
    auto plane1 = plane_through{p, q};
    auto plane2 = plane_through{r, s};

    auto iplane1 = ImplicitPlane{plane1};
    auto iplane2 = ImplicitPlane{plane2};
    printt(iplane1, iplane2);
    printt(intersection(make_point(0_i8, 42_i8), plane1));
    printt(intersection(make_point(3_i8, 42_i8), plane1));
    printt(intersection(iplane1, iplane2));
    printt(iplane1.contains(make_point(0_i8, 2_i8)));
    printt(iplane2.contains(make_point(0_i8, 2_i8)));
    print(plane2.contains(r), plane2.contains(s));
    print(iplane2.contains(r), iplane2.contains(s));
    printt(intersection(iplane1, iplane2).contains(make_point(0_i8, 2_i8)));
    printt(intersection(iplane1, iplane2, make_point(0_i8, 2_i8)));
    printt(intersection(iplane1, iplane2, make_point(1_i8, 2_i8)));

    printt(det(vec{2_i8, 3_i8}, vec{5_i8, 7_i8}));
    printt(det(vec{2_const, 3_const}, vec{5_const, 7_const}));
    printt(det(vec{2_const, 3_i8}, vec{5_const, 7_const}));
    printt(det(vec{2_const, 3_i8}, vec{5_const, 7_const}) == 0_const);
    printt(det(vec{2_const, 3_i8}, vec{5_const, 7_const}) == sum(-17_i8, 16_const));
    printt(3_i8 + integer{2_const - 3_const});

    print(Sum{42_i8} + Sum{28_const});
    printt(sum(5_const, 7_const, 11_i8, -12_const, 15_i16));
    printt(sum(5_const, 7_const, 11_i8, 9_const, 15_i16));
    printt(sum(5_const, 7_const, 16_const));
    printt(sum(sum(1_const, 2_i8), sum(3_const, 4_i8)));
    printt(sum(sum(1_const, 2_const), sum(3_const, 4_i8)));

    printt(sum(sum(1_const, 5_i8), sum(2_i8, 3_const)));

    sub_array<integer<8>, 6> array1{1_i8, 2_i8};
    sub_array<integer<8>, 6> array2{};
    array1.push_back(42);
    array2 = array1;
    array1.push_back(66);
    array1.emplace_back(111);
    sub_array<integer<8>, 4> array3{1_i8, 2_const, 3_i8, 5_i8};
    sub_array<integer<8>, 5> array4{array3};
    sub_array<integer<8>, 6> array5;
    array5 = array4;
    printt(array1, array2, array3, array4, array5);
    sub_array<constant<42>, 10> array6;

    set<vec<integer<8>>, 4> ps{vec{2_i8}, vec{3_i8}, vec{4_i8}};
    set<vec<integer<8>>, 5> qs{vec{3_i8}, vec{4_i8}, vec{5_i8}};
    set<vec<integer<16>, constant<42>>, 5> rs{vec{3_i16, 42_const}, vec{4_i16, 42_const}, vec{5_i16, 42_const}};
    set<vec<constant<4>, integer<8>>, 7> ss{vec{4_const, 7_i8}, vec{4_const, 42_i8}};
    // printt(ps, qs, intersection(ps, qs));
    // printt(rs, ss, intersection(rs, ss));
    printt(dot(vec{2_i8, 5_i8}, vec{7_i8, 3_i8}));

    set<vec<integer<8>>, 4> set1{vec{3_i8}, vec{5_i8}, vec{7_i8}};
    auto set2 = set{vec{42_i8}, vec{666_i16}};
    auto set3 = set{vec{28_i8}, vec{42_i16}};
    printt(set1, set2);
    printt(intersection(vec{555_i16}, vec{666_i16}));
    printt(intersection(vec{666_i16}, vec{666_i16}));
    printt(intersection(set2, vec{666_i16}));
    printt(intersection(vec{666_i16}, set2));
    printt(intersection(set1, set2));
    printt(intersection(set2, set3));
    printt(set{});

    print(quotient(13_i8, nonzero{5_i8}));
    print(quotient(15_const, 5_const));
    printt(lcm(10_const, 42_const));
    printt(gcd(i16{2 * 3 * 5 * 7}, i16{5 * 7 * 11}));
    printt(gcd(nonzero{4_const}, nonzero{6_const}));
    printt(gcd(30_const, 42_const, 66_const));

    oneof<integer<16>, integer<8>> meh{42_i8};
    printt(meh);
    print(proxy<oneof_unique<integer<1>, integer<2>, integer<1>>>{});

    auto point1 = proj{2_const, vec{10_i8, 14_i8}};
    auto point2 = proj{1_const, vec{5_i8, 7_i8}};
    printt(point1, point2, point1 <=> point2);
    auto dir1 = proj{0_const, vec{1_const, 0_const}};
    auto dir2 = proj{0_const, vec{2_const, 0_const}};
    printt(dir1, dir2, dir1 <=> dir2);
    printt(proj{6_const, vec{4_const, 8_const}}.repr());
    printt(gcd(3_const, 1_const));
    printt(gcd(1_const, 1_const));

    // affine vs projective space
    // projective space?
    // invariant that plane_through(p, q) always has p ≠ q, smart constructor returning variant

    return 0;
}
