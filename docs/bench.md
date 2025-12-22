## Benchmarks

Benchmarks run on a Dell M4700 laptop.
Intel i7-3740QM, 32 GB RAM. 
Ubuntu 20.04, Linux x86_64 5.4.

     make bootstrap bench
     make CELL=8 bootstrap bench
     make FORTH=gforth bench
     make FORTH=gforth-fast bench

m32 - McForth 32-bit version  
m64 - McForth 64-bit version

gforth 0.7.3 64-bit version

12/16/20 Using gcc, locals, catch/throw, dynamic dict

bench   | m32  | m64  | gforth | gforth-fast
------- | ---- | ---- | ------ | -----------
seive   | 1.24 | 1.08 | 1.10   | 0.60
bubble  | 1.85 | 1.21 | 1.69   | 0.91
fib     | 3.51 | 2.44 | 2.80   | 1.68
matrix  | 1.72 | 1.58 | 1.23   | 0.95
mm-rtcg | 1.53 | 1.35 | 1.57   | 1.55
