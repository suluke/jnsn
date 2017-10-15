#ifndef PARSING_AST_H
#define PARSING_AST_H

#include "parsing/ast_visitor.h"
#include "parsing/lexer.h"
#include <cassert>
#include <vector>

namespace parsing {
class ast_node_store;

/// An enum for all the ast node types
enum class ast_node_type {
#define NODE(NAME, CHILD_NODES) NAME##_ty,
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) NAME##_ty,
#include "parsing/ast.def"
};

/// A handle to ast nodes stored inside an ast_node_store
class ast_node_ref {
private:
  ast_node_store *store;
  size_t id;

protected:
  ast_node_type ty;
  ast_node_ref(ast_node_store &store, ast_node_type ty, size_t id)
      : store(&store), ty(ty), id(id) {}
  ast_node &get();
  const ast_node &get() const;

public:
  ast_node_ref() = default;
  ast_node_ref(const ast_node_ref &) = default;
  ast_node_ref(ast_node_ref &&) = default;
  ast_node_ref &operator=(ast_node_ref &&) = default;
  ast_node_ref &operator=(const ast_node_ref &) = default;
  ast_node &operator*();
  ast_node *operator->();
  const ast_node &operator*() const;
  const ast_node *operator->() const;
  operator bool() const { return store; }
};
template <class Ty> class typed_ast_node_ref : public ast_node_ref {
  bool is_consistent() const { return !*this || Ty::is_extended_by(ty); }
  friend class ast_node_store;
  typed_ast_node_ref(ast_node_store &store, ast_node_type ty, size_t id)
      : ast_node_ref(store, ty, id) {}

public:
  typed_ast_node_ref() = default;
  typed_ast_node_ref(const typed_ast_node_ref<Ty> &) = default;
  typed_ast_node_ref(typed_ast_node_ref<Ty> &&) = default;
  typed_ast_node_ref &operator=(typed_ast_node_ref<Ty> &&) = default;
  typed_ast_node_ref &operator=(const typed_ast_node_ref<Ty> &) = default;

  template<class Derived> typed_ast_node_ref(const typed_ast_node_ref<Derived> &o) : ast_node_ref(o) {
    assert(is_consistent());
  }
  template<class Derived> typed_ast_node_ref(typed_ast_node_ref<Derived> &&o) : ast_node_ref(o) {
    assert(is_consistent());
  }

  explicit typed_ast_node_ref(const ast_node_ref &o) : ast_node_ref(o) {
    assert(is_consistent());
  }
  explicit typed_ast_node_ref(ast_node_ref &&o) : ast_node_ref(o) {
    assert(is_consistent());
  }

  Ty &operator*() {
    assert(*this);
    assert(is_consistent());
    return static_cast<Ty &>(get());
  }
  Ty *operator->() {
    assert(*this);
    assert(is_consistent());
    return static_cast<Ty *>(&get());
  }
  const Ty &operator*() const {
    assert(*this);
    assert(is_consistent());
    return static_cast<const Ty &>(get());
  }
  const Ty *operator->() const {
    assert(*this);
    assert(is_consistent());
    return static_cast<const Ty *>(&get());
  }
};

/// Setting up node structs
#define NODE(NAME, CHILD_NODES)                                                \
  struct NAME##_node : public ast_node CHILD_NODES;
#define CHILDREN(...)                                                          \
  {                                                                            \
    __VA_ARGS__                                                                \
    void accept(ast_node_visitor_base &v) override { v.gen_result(*this); }    \
    void accept(const_ast_node_visitor_base &v) const override {               \
      v.gen_result(*this);                                                     \
    }                                                                          \
    static bool has_type(ast_node_type ty);                                    \
    static bool extends(ast_node_type ty);                                     \
    static bool is_extended_by(ast_node_type ty);                              \
  }
#define EXTENDS(BASE)                                                          \
public                                                                         \
  BASE##_node
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  struct NAME##_node : ANCESTOR CHILD_NODES;
#define MANY(OF, NAME) std::vector<typed_ast_node_ref<OF##_node>> NAME;
#define ONE(OF, NAME) typed_ast_node_ref<OF##_node> NAME;
#define STRING(NAME) string_table::entry NAME;
#define STRINGS(NAME) std::vector<string_table::entry> NAME;
#define MAYBE_STR(NAME) std::optional<string_table::entry> NAME;
#define MAYBE(OF, NAME) std::optional<typed_ast_node_ref<OF##_node>> NAME;
#include "parsing/ast.def"

/// The place where different nodes live
class ast_node_store {
  friend class ast_node_ref;
  ast_node &get(ast_node_type ty, size_t id);
  const ast_node &get(ast_node_type ty, size_t id) const;

#define NODE(NAME, CHILD_NODES) std::vector<NAME##_node> NAME##_vec;
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  std::vector<NAME##_node> NAME##_vec;
#include "parsing/ast.def"

public:
#define NODE(NAME, CHILD_NODES)                                                \
  typed_ast_node_ref<NAME##_node> make_##NAME() {                              \
    NAME##_vec.emplace_back();                                                 \
    return {*this, ast_node_type::NAME##_ty, NAME##_vec.size() - 1};           \
  }
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  typed_ast_node_ref<NAME##_node> make_##NAME() {                              \
    NAME##_vec.emplace_back();                                                 \
    return {*this, ast_node_type::NAME##_ty, NAME##_vec.size() - 1};           \
  }
#include "parsing/ast.def"

  void clear();
};

std::ostream &operator<<(std::ostream &, const ast_node_ref &);
std::ostream &operator<<(std::ostream &, const ast_node &);
template <class Ty>
std::ostream &operator<<(std::ostream &stream,
                         const typed_ast_node_ref<Ty> &ref) {
  stream << static_cast<const ast_node_ref &>(ref);
}
} // namespace parsing

#endif // PARSING_AST_H
