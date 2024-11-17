/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstddef>
#include <format>
#include <variant>

#include <SimpleFormat.h>

namespace Arwen {

template<typename TagType, typename... Types>
class TaggedUnion {
public:
    static_assert(sizeof...(Types) > 0);
    using Payload = std::variant<Types...>;

    template<TagType Tag>
    constexpr TaggedUnion &emplace()
    {
        m_payload.template emplace<static_cast<std::size_t>(Tag)>();
        return *this;
    }

    template<TagType Tag, typename... Args>
    constexpr TaggedUnion &emplace(Args... args)
    {
        m_payload.template emplace<static_cast<std::size_t>(Tag)>(std::forward<Args>(args)...);
        return *this;
    }

    constexpr TaggedUnion() = default;

    explicit TaggedUnion(TagType tag)
    {
        assert(static_cast<size_t>(tag) < sizeof...(Types));
        set<sizeof...(Types) - 1>(tag);
    }

    template<typename... Args>
    constexpr explicit TaggedUnion(TagType tag, Args&&... args)
    {
        assert(static_cast<size_t>(tag) < sizeof...(Types));
        set<sizeof...(Types) - 1>(tag, std::forward<Args>(args)...);
    }

    [[nodiscard]] constexpr TagType tag() const
    {
        return static_cast<TagType>(m_payload.index());
    }

    [[nodiscard]] constexpr bool operator<(TaggedUnion const &rhs) const
    {
        return this->m_payload < rhs.m_payload;
    }

    [[nodiscard]] constexpr bool operator==(TaggedUnion const &rhs) const
    {
        return this->m_payload == rhs.m_payload;
    }

    template<TagType Tag>
    constexpr std::variant_alternative_t<static_cast<std::size_t>(Tag), Payload> get() const
    {
        return std::get<static_cast<std::size_t>(Tag)>(m_payload);
    }

    constexpr                operator TagType() const { return tag(); }
    constexpr Payload       &payload() { return m_payload; }
    constexpr Payload const &payload() const { return m_payload; }

private:
    Payload m_payload;

    template<size_t Idx>
    constexpr TaggedUnion &set(TagType tag)
    {
        if (static_cast<size_t>(tag) == Idx) {
            return emplace<static_cast<TagType>(Idx)>();
        }
        if constexpr (Idx > 0) {
            return set<Idx - 1>(tag);
        }
        assert(false);
        return *this;
    }

    template<size_t Idx, typename... Args>
    constexpr TaggedUnion &set(TagType tag, Args&& ...args)
    {
        if (static_cast<size_t>(tag) == Idx) {
            if constexpr (std::is_constructible_v<std::variant_alternative_t<Idx, Payload>, Args...>) {
                m_payload.template emplace<Idx>(std::forward<Args>(args)...);
            }
            return *this;
        }
        if constexpr (Idx > 0) {
            return set<Idx - 1>(tag, std::forward<Args>(args)...);
        }
        assert(false);
        return *this;
    }
};

}

template<typename TagType, typename... Types>
struct std::formatter<Arwen::TaggedUnion<TagType, Types...>, char> : public Arwen::SimpleFormatParser {
    using TU = Arwen::TaggedUnion<TagType, Types...>;
    template<class FmtContext>
    FmtContext::iterator format(TU const &tagged_union, FmtContext &ctx) const
    {
        std::ostringstream out;
        std::visit(
            [&out](auto const &tagged_value) {
                using T = std::decay<decltype(tagged_value)>;
                if constexpr (std::formattable<T, char>) {
                    out << std::format("{}", tagged_value);
                } else {
                    out << tagged_value;
                }
            },
            tagged_union.payload());
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
