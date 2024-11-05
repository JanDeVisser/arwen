/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <set>

namespace Arwen {

template<typename T>
class Set {
public:
    Set() = default;
    Set(Set<T> const &other) = default;

    ssize_t size() const
    {
        return static_cast<ssize_t>(set.size());
    }

    bool empty() const
    {
        return set.empty();
    }

    bool has(T const &elem) const
    {
        return set.contains(elem);
    }

    void add(T const &elem)
    {
        set.insert(elem);
    }

    void remove(T const &elem)
    {
        set.erase(elem);
    }

    void union_with(Set<T> const &other)
    {
        for (auto elem : other.set) {
            add(elem);
        }
    }

    void intersect(Set<T> const &other)
    {
        std::erase_if(set, [&other](auto const &elem) {
            return !other.set.contains(elem);
        });
    }

    void minus(Set<T> const &other)
    {
        std::erase_if(set, [&other](auto const &elem) {
            return other.set.contains(elem);
        });
    }

    Set<T> operator+(Set<T> other) const
    {
        Set<T> ret { *this };
        ret.union_with(other);
        return ret;
    }

    Set<T> operator-(Set<T> other) const
    {
        Set<T> ret { *this };
        ret.minus(other);
        return ret;
    }

    Set<T> operator*(Set<T> other) const
    {
        Set<T> ret { *this };
        ret.intersect(other);
        return ret;
    }

    bool operator[](T const& elem) const
    {
        return set.contains(elem);
    }

    auto begin()
    {
        return set.begin();
    }

    auto end()
    {
        return set.end();
    }

    auto cbegin() const
    {
        return set.cbegin();
    }

    auto cend() const
    {
        return set.cend();
    }

private:
    std::set<T> set;
    friend struct std::formatter<Arwen::Set<T>, char>;
};

}
