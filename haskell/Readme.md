# Haskell

An attempt at implementing the Benchmark in Haskell (for funsies).

This Benchmark aims to work without installing any further dependencies beyond base.
It makes use of [forkIO](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Concurrent.html#v:forkIO) for lightweight, unbound threads and uses [Control.Concurrent.Chan](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Concurrent-Chan.html) to await the sleeping threads so that they're not collected earlier.

## Results

### Arch Linux

#### GHC 9.2.8

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

#### GHC 9.6.2

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

### Windows 11 with WSL Ubuntu and GHC 9.2.8

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

### MacOS with GHC 9.6.2

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