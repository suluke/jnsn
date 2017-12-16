#ifndef JNSN_IR_TYPES_H
#define JNSN_IR_TYPES_H
namespace jnsn {

// runtime types
class type {
#define TYPE(NAME) friend class NAME##_type;
#define SUBTYPE(NAME, BASE) TYPE(NAME)
#include "jnsn/ir/types.def"
  template <class ty> friend bool isa(type valty);
  enum kind {
#define TYPE(NAME) NAME##_ty,
#define SUBTYPE(NAME, BASE) TYPE(NAME)
#include "jnsn/ir/types.def"
  };
  const kind ty;
  type(kind ty) : ty(ty) {}

public:
  type(const type &) = default;
  type(type &&) = default;
  bool operator==(const type &o) { return ty == o.ty; }
};
#define TYPE(NAME)                                                             \
  struct NAME##_type : public type {                                           \
    template <class ty> friend bool isa(type valty);                           \
    static constexpr type::kind ty = type::NAME##_ty;                          \
                                                                               \
  public:                                                                      \
    static inline type create() { return type(type::NAME##_ty); }              \
  };
#define SUBTYPE(NAME, BASE) TYPE(NAME)
#include "jnsn/ir/types.def"

/// type introspection support for types
template <class ty> bool isa(type valty) {
  if (valty.ty == ty::ty)
    return true;
#define SUBTYPE(TY, BASE)                                                      \
  if (valty.ty == type::TY##_ty)                                               \
    return isa<ty>(BASE##_type::create());
#include "jnsn/ir/types.def"
  return false;
}

} // namespace jnsn
#endif // JNSN_IR_TYPES_H
