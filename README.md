operf-micro
===========

A set of micro-benchmarks for OCaml compiler

### Build and Install

* Modify `Makefile.conf`
* Build:

```
make 
```

* Install:

```
(sudo) make install
```

### Run 

If you are in OCaml sources, just use:
```
operf-micro init SomeNameForTheseTests
```
otherwise, create a directory for the tests and provide the bin/ directory where OCaml is installed:
```
operf-micro init --bin-dir $HOME/.opam/4.01.0/bin/ TestsOn-4.01.0
```
This command will create a sub-directory `.operf` in the local directory when benchmarks sources are copied, and will later be built and executed.

Now, we can build the tests:
```
operf-micro build
```
We can list the available tests:
```
operf-micro list
```
We can run a few tests, for example the ones in "list":
```
operf-micro run list
```
`operf-micro` runs the benchmarks several times, until the variance is low enough for the results to be significant.

Now, we can display the results:
```
operf-micro results TestsOn-4.01.0
```
and we get the output:
```
TestsOn-4.01.0 2015-02-04_11-45-24:
  list:
    group fold_left add
      tail_rec: 26.18
      while: 29.94
      while_exn: 120.04
      
    group fold_left add_float
      tail_rec: 26.14
      while: 29.82
      while_exn: 118.87
      
    group interval
      direct: 22.58
      tail_rec: 22.03
      tail_rec_with_closure: 24.59
      
    group map succ
      direct: 25.76
      closure: 29.72
      tail_rec: 36.34
      
    group rev
      rec: 21.32
      rev_while: 23.07
      
    group rev_map succ
      rev_map_tail_rec succ: 25.19
      rev_map_while succ: 28.13
      
```

If we have done the same for another version of OCaml, for example TestsOn-4.02.0, we can compare them with:
```
operf-micro compare
```
