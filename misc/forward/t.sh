#!/bin/sh
clang-tidy -header-filter=.* --checks=-clang-analyzer-security.insecureAPI.strcpy,-clang-analyzer-security.insecureAPI.DeprecatedOrUnsafeBufferHandling,-clang-analyzer-security.insecureAPI.UncheckedReturn p4emu_full.c
cppcheck --suppress=nullPointerOutOfMemory p4emu_full.c
