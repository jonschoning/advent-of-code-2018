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

### Day 4

```
$ make day4p1 m=b; make day4p2 m=b 
.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 4 1 input/day4.txt 
benchmarking...
time                 2.419 ms   (2.403 ms .. 2.441 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.409 ms   (2.405 ms .. 2.416 ms)
std dev              17.94 μs   (11.69 μs .. 29.31 μs)

.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 4 2 input/day4.txt 
benchmarking...
time                 2.432 ms   (2.418 ms .. 2.454 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 2.446 ms   (2.432 ms .. 2.474 ms)
std dev              65.98 μs   (47.44 μs .. 94.93 μs)
variance introduced by outliers: 14% (moderately inflated)

```

### Day 5

```
$ make day5p1 m=b; make day5p2 m=b  
.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 5 1 input/day5.txt 
benchmarking...
time                 3.140 ms   (3.131 ms .. 3.151 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.129 ms   (3.122 ms .. 3.137 ms)
std dev              22.96 μs   (18.07 μs .. 29.49 μs)

.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 5 2 input/day5.txt 
benchmarking...
time                 93.83 ms   (93.60 ms .. 94.02 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 94.09 ms   (93.97 ms .. 94.24 ms)
std dev              223.6 μs   (170.7 μs .. 311.1 μs)
```

### Day 6


```
make day6p1 m=b; make day6p2 m=b                                                                                    ✘
.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 6 1 input/day6.txt 
benchmarking...
ktime                 116.9 ms   (113.5 ms .. 121.3 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 118.2 ms   (116.2 ms .. 121.1 ms)
std dev              3.688 ms   (2.019 ms .. 6.358 ms)
variance introduced by outliers: 11% (moderately inflated)

.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 6 2 input/day6.txt 
benchmarking...
time                 53.45 ms   (53.08 ms .. 54.09 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 53.67 ms   (53.37 ms .. 54.29 ms)
std dev              798.8 μs   (463.0 μs .. 1.279 ms)
```

### Day 8

```
make day8p1 m=b; make day8p2 m=b                                                                                    ✘
.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 8 1 input/day8.txt 
benchmarking...
time                 2.332 ms   (2.320 ms .. 2.344 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.343 ms   (2.338 ms .. 2.350 ms)
std dev              18.32 μs   (14.44 μs .. 22.53 μs)

.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin/AOC2018 b 8 2 input/day8.txt 
benchmarking...
time                 1.970 ms   (1.950 ms .. 1.990 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.968 ms   (1.961 ms .. 1.977 ms)
std dev              26.91 μs   (18.41 μs .. 44.07 μs)
```
