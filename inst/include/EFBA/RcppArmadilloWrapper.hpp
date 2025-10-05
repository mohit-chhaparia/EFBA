#pragma once

// Silence the "unknown warning option" that can be triggered by
// R headers on some Apple/LLVM Clang builds.
#if defined(__clang__) && defined(__has_warning)
  #if __has_warning("-Wunknown-warning-option")
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wunknown-warning-option"
  #endif
#endif

#include <RcppArmadillo.h>

#if defined(__clang__) && defined(__has_warning)
  #if __has_warning("-Wunknown-warning-option")
    #pragma clang diagnostic pop
  #endif
#endif
