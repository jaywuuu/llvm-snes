//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// WARNING: This test was generated by generate_feature_test_macro_components.py
// and should not be edited manually.
//
// clang-format off

// <optional>

// Test the feature test macros defined by <optional>

/*  Constant              Value
    __cpp_lib_optional    201606L [C++17]
*/

#include <optional>
#include "test_macros.h"

#if TEST_STD_VER < 14

# ifdef __cpp_lib_optional
#   error "__cpp_lib_optional should not be defined before c++17"
# endif

#elif TEST_STD_VER == 14

# ifdef __cpp_lib_optional
#   error "__cpp_lib_optional should not be defined before c++17"
# endif

#elif TEST_STD_VER == 17

# ifndef __cpp_lib_optional
#   error "__cpp_lib_optional should be defined in c++17"
# endif
# if __cpp_lib_optional != 201606L
#   error "__cpp_lib_optional should have the value 201606L in c++17"
# endif

#elif TEST_STD_VER == 20

# ifndef __cpp_lib_optional
#   error "__cpp_lib_optional should be defined in c++20"
# endif
# if __cpp_lib_optional != 201606L
#   error "__cpp_lib_optional should have the value 201606L in c++20"
# endif

#elif TEST_STD_VER > 20

# ifndef __cpp_lib_optional
#   error "__cpp_lib_optional should be defined in c++2b"
# endif
# if __cpp_lib_optional != 201606L
#   error "__cpp_lib_optional should have the value 201606L in c++2b"
# endif

#endif // TEST_STD_VER > 20

int main(int, char**) { return 0; }
