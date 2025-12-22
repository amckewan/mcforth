FORTH = ../forth
#FORTH = gforth-fast -e "warnings off"
TIME = time -f "%U"

all:
	$(TIME) $(FORTH) ./sieve.f -e "main bye"
	$(TIME) $(FORTH) ./bubble-sort.f -e "main bye"
	$(TIME) $(FORTH) ./fib.f -e "main bye"
	$(TIME) $(FORTH) -m2M ./matrix-mult.f -e "main bye"
	$(TIME) $(FORTH) -m32M ./mm-rtcg.f -e "main bye"

