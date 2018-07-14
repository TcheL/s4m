# s4m

`s4m` **s**eeks out the **m**ean, **m**edian, **m**inimum and **m**aximum of a data set.

## License

[The MIT License](http://tchel.mit-license.org)

## Examples

Although the statically compiled binary executable file `s4m`  is available on Linux platform, you can compile the source file `s4m.F90` on your system.

Help info. can be obtained by `./s4m -h`:

```shell
Usage:
  s4m DataPoint1 DataPoint2 [ DataPoint3 ... ]
    directly read data set from command line arguments.
  s4m -f SetSize FileName
    read data set from specific input file.
```

As shown above, to seek out "4m" of the data set $[4, 9, 5, 3, 10, 21]$, we can run:

```shell
$ ./s4m 4 9 5 3 10 21
```

Besides, to seek out "4m" of the set of 3000 data points derived from the file `rand.dat`, we can run:

```shell
$ ./s4m -f 3000 rand.dat
```

## Author

Tche LIU, seistche@gmail.com, USTC