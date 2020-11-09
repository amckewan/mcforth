## Benchmarks

bench | gforth | gforth-fast | 10/31 | 11/1 | 11/2 | 11/8 | m64
--- | ------| ---- | --- | --- | --- | --- | ---
seive | 1.15 | 0.61 | 3.24 | 3.61 | 1.97 | 1.82 | 2.26
bubble | 1.72 | 0.94 | 12.05 | 11.37 | 1.92 | 2.63 | 2.99
fib | 2.86 | 1.73 | 8.56 | 4.69 | 3.18 | 4.56 | 4.37

## Builds

ab5ce02 Sat Oct 31 09:09:37 2020 - inlining

00e26ba Sun Nov 1 18:10:24 2020 - optimized

a8a1729 Mon Nov 2 15:59:06 2020 - more opt, -Ofast

