## Benchmarks

12/15/20 Using gcc, locals, catch/throw, dynamic dict

Interesting that removing processing the -m option
shows improved times: 1.12, 1.30, 2.81. Yet the generated
code is almost identical. A puzzle indeed.

bench  | m32  | m64  | gforth | gforth-fast
------ | ---- | ---- | ------ | -----------
seive  | 1.30 | 1.10 | 1.15   | 0.61
bubble | 1.88 | 1.20 | 1.72   | 0.94
fib    | 3.52 | 2.42 | 2.86   | 1.73
