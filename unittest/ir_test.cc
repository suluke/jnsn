#include "jnsn/ir/ir_context.h"
#include "gtest/gtest.h"
#include <sstream>

using namespace jnsn;

TEST(ir_test, type_isa) {
  ASSERT_TRUE(isa<value_type>(addr_type::create()));  // inheritance
  ASSERT_TRUE(isa<value_type>(value_type::create())); // reflexive
  ASSERT_FALSE(isa<addr_type>(bool_type::create()));  // same pos in hierarchy
  ASSERT_FALSE(isa<register_type>(object_type::create()));
}
TEST(ir_test, value_isa) {
  ir_context ctx;
  auto &str = *ctx.get_c_str_val("test");
  auto &num = *ctx.get_c_num_val(1.);
  add_inst a{ctx};
  ASSERT_TRUE(isa<value>(a));  // inheritance
  ASSERT_TRUE(isa<add_inst>(a)); // reflexive
  ASSERT_FALSE(isa<sub_inst>(a));  // same pos in hierarchy
  ASSERT_TRUE(isa<value>(str));
  ASSERT_TRUE(isa<global_value>(str));
  ASSERT_TRUE(isa<c_str_val>(str));
  ASSERT_TRUE(isa<value>(num));
  ASSERT_TRUE(isa<constant>(num));
  ASSERT_TRUE(isa<c_num_val>(num));
}
