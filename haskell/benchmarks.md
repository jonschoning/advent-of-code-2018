# Benchmarks

CPU model name : Intel(R) Core(TM) i7-2620M CPU @ 2.70GHz

### Day 1

```
[130] $ make day1p1 m=b; make day1p2 m=b                                                         ✘
.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 1 1 input/day1.txt 
benchmarking...
time                 35.83 μs   (35.69 μs .. 36.05 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 36.07 μs   (35.89 μs .. 36.39 μs)
std dev              751.5 ns   (476.8 ns .. 1.265 μs)
variance introduced by outliers: 18% (moderately inflated)

.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 1 2 input/day1.txt 
benchmarking...
time                 16.90 ms   (16.86 ms .. 16.94 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 16.94 ms   (16.91 ms .. 16.97 ms)
std dev              71.25 μs   (52.61 μs .. 95.53 μs)
```

### Day 2

```
$ make day2p1 m=b; make day2p2 m=b
.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 2 1 input/day2.txt 
benchmarking...
time                 453.8 μs   (453.1 μs .. 454.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 453.9 μs   (453.5 μs .. 454.8 μs)
std dev              1.765 μs   (1.065 μs .. 3.122 μs)

.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 2 2 input/day2.txt 
benchmarking...
time                 6.545 ms   (6.516 ms .. 6.585 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 6.515 ms   (6.505 ms .. 6.539 ms)
std dev              42.86 μs   (21.41 μs .. 80.65 μs)
```

### Day 3

```
$ make day3p1 m=b; make day3p2 m=b                                                         ✘
.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 3 1 input/day3.txt 
benchmarking...
time                 327.5 ms   (312.8 ms .. 353.5 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 345.0 ms   (335.8 ms .. 361.1 ms)
std dev              15.65 ms   (1.205 ms .. 20.09 ms)
variance introduced by outliers: 19% (moderately inflated)

.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 3 2 input/day3.txt 
benchmarking...
time                 358.0 ms   (338.0 ms .. 382.4 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 384.2 ms   (371.0 ms .. 403.6 ms)
std dev              18.38 ms   (1.883 ms .. 23.38 ms)
variance introduced by outliers: 19% (moderately inflated)
```
