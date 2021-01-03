FORTH = ../forth
#FORTH = gforth-fast -e "warnings off"
TIME = time -f "%U"

all:
	$(TIME) $(FORTH) ./sieve.f -e "main bye"
	$(TIME) $(FORTH) ./bubble-sort.f -e "main bye"
	$(TIME) $(FORTH) ./fib.f -e "main bye"
#	$(TIME) $(FORTH) ./matrix-mult.f -e "main bye"
#	$(TIME) $(FORTH) ./mm-rtcg.f -e "main bye"
clean:
