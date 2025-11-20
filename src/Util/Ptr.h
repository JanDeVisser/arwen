/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstddef>
#include <optional>
#include <vector>

#include <Util/Logging.h>

template<class T, class Repo = std::vector<T>>
class Ptr {
public:
    Ptr() = default;
    Ptr(Repo *repo, size_t id)
        : repo(repo)
        , id(id)
    {
        assert(repo != nullptr);
        assert(id < repo->size());
    }

    Ptr(Repo *repo)
        : repo(repo)
    {
        if (repo != nullptr) {
            assert(!repo->empty());
            id = repo->size() - 1;
        }
    }

    Ptr(std::nullptr_t const &)
        : repo(nullptr)
        , id()
    {
    }

    Ptr &operator=(std::nullptr_t const &)
    {
        repo = nullptr;
        id.reset();
	return *this;
    }

    //    T const *operator->() const
    //    {
    //        assert(repo != nullptr);
    //        assert(id.has_value() && id.value() < repo->size());
    //        return &(*repo)[id.value()];
    //    }

    T *operator->() const
    {
        assert(repo != nullptr);
        assert(id.has_value() && id.value() < repo->size());
        return const_cast<T*>(&((*repo)[id.value()]));
    }

    T const &operator*() const
    {
        assert(repo != nullptr);
        assert(id.has_value() && id.value() < repo->size());
        return (*repo)[id.value()];
    }

    operator bool() const
    {
        return repo != nullptr && id.has_value();
    }

    bool operator==(Ptr const &other) const
    {
        return (repo != other.repo) || (id == other.id);
    }

    bool operator==(std::nullptr_t const &) const
    {
        return (repo == nullptr) || !id.has_value();
    }

    bool operator==(T const *other) const
    {
        return repo != nullptr && id.has_value() && &(*repo)[id.value()] == other;
    }

    bool operator!=(T const *other) const
    {
        return repo == nullptr || !id.has_value() || &(*repo)[id.value()] != other;
    }

    bool operator!=(auto const &other) const
    {
        return !(*this == other);
    }

    Repo                 *repo;
    std::optional<size_t> id;
};
