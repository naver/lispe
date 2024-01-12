# Compiling with blas

BLAS stands for Basic Linear Algebra.

We have two different ways to compile with LispE:

1. The version based on the C++ API: see https://icl.bitbucket.io/blaspp/index.html
1. The version (by default) based on the C API: apt-get install libblas-dev

If you want to compile with BLAS++, rename the file: Makefile.for_blaspp into Makefile...
