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
