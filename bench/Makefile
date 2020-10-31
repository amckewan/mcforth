FORTH = ../forth
#FORTH = gforth -e "warnings off"
TIME = time -p

all:
	$(TIME) $(FORTH) ./sieve.fs -e "main bye"
	$(TIME) $(FORTH) ./bubble-sort.fs -e "main bye"
	$(TIME) $(FORTH) ./fib.fs -e "main bye"
#	$(TIME) $(FORTH) ./matrix-mult.fs -e "main bye"
#	$(TIME) $(FORTH) ./ssieve-a.frt -e "?silent on list-primes 1 20000000 bye"
#	$(TIME) $(FORTH) ./mm-rtcg.fs -e "main bye"
clean:
