null :=
ifeq ($(null),$(m))
  m = p
endif

aoc=.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018
aoc_m=$(aoc) $(m)
stack = stack

ghcflags = --ghc-options -O2
targs = --test-arguments=--match
rtsopts = 
ifeq (p, $(m))
  rtsopts = +RTS -s
endif

doctest = doctest --fast -XOverloadedStrings

.PHONY: default clean

default:
	$(stack) build $(ghcflags)

clean: 
	$(stack) clean

testall: 
	$(stack) test --fast

$(aoc): app/Main.hs
	$(stack) build $(ghcflags)

day1test:
	$(stack) test $(targs)=Day1
day1test-doc:
	$(stack) exec -- $(doctest)  src/Day1.hs
day1p1: $(aoc)
	$(aoc_m) 1 1 input/day1.txt $(rtsopts)
day1p2: $(aoc)
	$(aoc_m) 1 2 input/day1.txt $(rtsopts)


day2test:
	$(stack) test $(targs)=Day2
day2test-doc:
	$(stack) exec -- $(doctest) src/Day2.hs
day2p1: $(aoc)
	$(aoc_m) 2 1 input/day2.txt $(rtsopts)
day2p2: $(aoc)
	$(aoc_m) 2 2 input/day2.txt $(rtsopts)


day3test:
	$(stack) test $(targs)=Day3
day3test-doc:
	$(stack) exec -- $(doctest) src/Day3.hs
day3p1: $(aoc)
	$(aoc_m) 3 1 input/day3.txt $(rtsopts)
day3p2: $(aoc)
	$(aoc_m) 3 2 input/day3.txt $(rtsopts)


day4test:
	$(stack) test $(targs)=Day4
day4test-doc:
	$(stack) exec -- $(doctest) src/Day4.hs
day4p1: $(aoc)
	$(aoc_m) 4 1 input/day4.txt $(rtsopts)
day4p2: $(aoc)
	$(aoc_m) 4 2 input/day4.txt $(rtsopts)


day5test:
	$(stack) test $(targs)=Day5
day5test-doc:
	$(stack) exec -- $(doctest) src/Day5.hs
day5p1: $(aoc)
	$(aoc_m) 5 1 input/day5.txt $(rtsopts)
day5p2: $(aoc)
	$(aoc_m) 5 2 input/day5.txt $(rtsopts)


day6test:
	$(stack) test $(targs)=Day6
day6test-doc:
	$(stack) exec -- $(doctest) src/Day6.hs
day6p1: $(aoc)
	$(aoc_m) 6 1 input/day6.txt $(rtsopts)
day6p2: $(aoc)
	$(aoc_m) 6 2 input/day6.txt $(rtsopts)


# day7test:
# 	$(stack) test $(targs)=Day7
# day7test-doc:
# 	$(stack) exec -- $(doctest) src/Day7.hs
# day7p1: $(aoc)
# 	$(aoc_m) 7 1 input/day7.txt $(rtsopts)
# day7p2: $(aoc)
# 	$(aoc_m) 7 2 input/day7.txt $(rtsopts)

day8test:
	$(stack) test $(targs)=Day8 --fast
day8test-doc:
	$(stack) exec -- $(doctest) src/Day8.hs
day8p1: $(aoc)
	$(aoc_m) 8 1 input/day8.txt $(rtsopts)
day8p2: $(aoc)
	$(aoc_m) 8 2 input/day8.txt $(rtsopts)


day9test:
	$(stack) test $(targs)=Day9 --fast
day9test-doc:
	$(stack) exec -- $(doctest) src/Day9.hs
day9p1: $(aoc)
	$(aoc_m) 9 1 input/day9.txt $(rtsopts)
day9p2: $(aoc)
	$(aoc_m) 9 2 input/day9.txt $(rtsopts)


day10test:
	$(stack) test $(targs)=Day10 --fast
day10test-doc:
	$(stack) exec -- $(doctest) src/Day10.hs
day10p1: $(aoc)
	$(aoc_m) 10 1 input/day10.txt $(rtsopts)
day10p2: $(aoc)
	$(aoc_m) 10 2 input/day10.txt $(rtsopts)
