# Haskell

An attempt at implementing the Benchmark in Haskell (for funsies).

This Benchmark aims to work without installing any further dependencies beyond base.
It makes use of [forkIO](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Concurrent.html#v:forkIO) for lightweight, unbound threads and uses [Control.Concurrent.Chan](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Concurrent-Chan.html) to await the sleeping threads so that they're not collected earlier.

## Results

### MainV5

In an attempt to further explore the direction of semaphores I was wondering if an [STM](https://hackage.haskell.org/package/stm-2.5.1.0/docs/Control-Concurrent-STM.html) [TVar](https://hackage.haskell.org/package/stm-2.5.1.0/docs/Control-Concurrent-STM-TVar.html) would work better.

On Arch Linux with GHC 9.6.2,
compiled with `ghc -O2 -o Main MainV5.hs`:

```shell
./run.sh
Running 1 task(s)
3780
Running 10000 task(s)
21504
Running 100000 task(s)
205888
Running 1000000 task(s)
1397188
./run.sh  601.22s user 2.00s system 93% cpu 10:42.63 total
```

### MainV4

In contrast to previous variants MainV4 uses [General Quantity Semaphores (QSemN)](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Concurrent-QSemN.html) to track the number of finished tasks. The hope is that by having only one shared structure to count finished tasks we could further cut down on memory usage.

On Arch Linux with GHC 9.6.2,
compiled with `ghc -O2 -o Main MainV4.hs`:

```shell
./run.sh
Running 1 task(s)
3584
Running 10000 task(s)
23504
Running 100000 task(s)
233348
Running 1000000 task(s)
2470020
./run.sh  586.33s user 2.42s system 93% cpu 10:27.97 total
```

On Arch Linux (8 threads) with GHC 9.6.2,
compiled with `ghc -O2 -o Main -threaded -rtsopts -with-rtsopts=-N MainV4.hs`:

```shell
./run.sh
Running 1 task(s)
10600
Running 10000 task(s)
59628
Running 100000 task(s)
766272
Running 1000000 task(s)
9342884
./run.sh  35.09s user 15.51s system 105% cpu 48.171 total
```

### MainV3

This variant works just like MainV2 but uses [TChan](https://hackage.haskell.org/package/stm-2.5.1.0/docs/Control-Concurrent-STM-TChan.html) from the [software transactional memory](https://hackage.haskell.org/package/stm-2.5.1.0/docs/Control-Concurrent-STM.html) module.

It also does away with separate channels and instead
has a helper function `go` that reads from the shared channel the desired number of times.

On Arch Linux with GHC 9.6.2,
compiled with `ghc -O2 -o Main MainV3.hs`:

```shell
./run.sh
Running 1 task(s)
3908
Running 10000 task(s)
21632
Running 100000 task(s)
203716
Running 1000000 task(s)
2408320
./run.sh  590.57s user 2.31s system 93% cpu 10:32.52 total
```

### MainV2

Instead of placing the chans in a list like MainV1 this version recursively calls a helper function `go` to establish a chain of function calls each waiting on the nested call to `go` and on it's IO threads channel.

My speculation is that this variant uses less memory because it does away with the list structure.

On Arch Linux with GHC 9.6.2,
compiled with `ghc -O2 -o Main MainV2.hs`:

```shell
./run.sh
[1 of 2] Compiling Main             ( Main.hs, Main.o ) [Source file changed]
[2 of 2] Linking Main [Objects changed]
Running 1 task(s)
3652
Running 10000 task(s)
22592
Running 100000 task(s)
93892
Running 1000000 task(s)
465220
./run.sh  776.18s user 1.47s system 95% cpu 13:38.33 total
```

Another run at a later point in time:

```shell
./run.sh
Running 1 task(s)
3584
Running 10000 task(s)
22656
Running 100000 task(s)
94532
Running 1000000 task(s)
462912
./run.sh  783.07s user 1.68s system 95% cpu 13:45.56 total
```

On Arch Linux (8 threads) with GHC 9.6.2,
compiled with `ghc -O2 -o Main -threaded -rtsopts -with-rtsopts=-N MainV2.hs`:

```shell
./run.sh
Running 1 task(s)
8392
Running 10000 task(s)
66356
Running 100000 task(s)
730964
Running 1000000 task(s)
9611132
./run.sh  29.94s user 14.20s system 96% cpu 45.933 total
```

### MainV1

This approach uses a [list of chans](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Concurrent-Chan.html), where each IO thread sends a `()` which is read sequentially in the main thread.

On Arch Linux with GHC 9.2.8:

```shell
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
Running 1 task(s)
3268
Running 10000 task(s)
29636
Running 100000 task(s)
98624
Running 1000000 task(s)
575428
./run.sh  788.83s user 1.83s system 95% cpu 13:52.27 total
```

On Arch Linux with GHC 9.6.2:

```shell
[1 of 2] Compiling Main             ( Main.hs, Main.o )
[2 of 2] Linking Main [Objects changed]
Running 1 task(s)
3652
Running 10000 task(s)
29824
Running 100000 task(s)
104516
Running 1000000 task(s)
566212
./run.sh  758.96s user 1.60s system 94% cpu 13:21.68 total
```

On Windows 11 with WSL Ubuntu and GHC 9.2.8:

```shell
Running 1 task(s)
3608
Running 10000 task(s)
29772
Running 100000 task(s)
133816
Running 1000000 task(s)
694560
```

On MacOS with GHC 9.6.2:

```shell
./run-gtime.sh
Running 1 task(s)
10048
Running 10000 task(s)
32416
Running 100000 task(s)
135856
Running 1000000 task(s)
608224
```