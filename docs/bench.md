MCFORTH ab5ce02 Sat Oct 31 09:09:37 2020

time -p ../forth ./sieve.fs -e "main bye"
Including ./sieve.fs
real 3.24
user 3.24
sys 0.00

time -p ../forth ./bubble-sort.fs -e "main bye"
Including ./bubble-sort.fs
real 12.05
user 12.05
sys 0.00

time -p ../forth ./fib.fs -e "main bye"
Including ./fib.fs
real 8.56
user 8.56
sys 0.00

OPTIMIZED (not much better, sadly )

time -p ../forth ./sieve.fs -e "main bye"
Including ./sieve.fs
real 3.61
user 3.61
sys 0.00
time -p ../forth ./bubble-sort.fs -e "main bye"
Including ./bubble-sort.fs
real 11.37
user 11.37
sys 0.00
time -p ../forth ./fib.fs -e "main bye"
Including ./fib.fs
real 4.69
user 4.69
sys 0.00

-------------------

Gforth 0.7.3

time -p gforth -e "warnings off" ./sieve.fs -e "main bye"
real 1.15
user 1.15
sys 0.00

time -p gforth -e "warnings off" ./bubble-sort.fs -e "main bye"
real 1.72
user 1.72
sys 0.00

time -p gforth -e "warnings off" ./fib.fs -e "main bye"
real 2.86
user 2.86
sys 0.00

------------

time -p gforth -e "warnings off" ./matrix-mult.fs -e "main bye"
real 1.25
user 1.25
sys 0.00

time -p gforth -e "warnings off" ./ssieve-a.frt -e "?silent on list-primes 1 20000000 bye"
real 0.31
user 0.31
sys 0.00
