# angicart

Angicart analyses 3d radiographic images of blood vessels to determine the
centerlines, topology, radius, length, and volume of blood vessel segments, as
applied in 
[Newberry MG, Ennis DB, Savage VM (2015) Testing Foundations of Biological Scaling Theory Using Automated Measurements of Vascular Networks. PLoS Comput Biol 11(8): e1004455. doi:10.1371/journal.pcbi.1004455](https://dx.doi.org/10.1371/journal.pcbi.1004455)

## Usage

Angicart is composed of a series of command-line programs that each do a small
task.  These programs are `pngs2pls`, `pls2pg`, `skeletonize`, `vis`, and
`graphdisplay`.  To build a program, invoke `make.sh` as

```
./make.sh graphdisplay native
```

which will create an executable `graphdisplay.native`.  Any program may be
invoked to display its options, eg:

```
$ ./skeletonize.native --help
Usage: skeletonize -o output [-f] lcc.pg
  Skeletonize a connected pointgraph
  -f pointgraph, created by output_value
  -o Output file
  -help  Display this list of options
  --help  Display this list of options
```

The behavior of each program in the context of the analysis of Newberry et al.
2015 is described in the example.  The invocations in the example can be
adapted to combine the programs in unanticipated ways.

## Example

The `example` directory contains scripts and input data to reproduce the
analysis of the original paper.  This shows how angicart works in practice, and
it can be adapted to a variety of situations.  For details, see
`example/README.md`.

## Citing angicart

For citing angicart in academic work, please cite the paper that introduced it,

> [Newberry MG, Ennis DB, Savage VM (2015) Testing Foundations of Biological Scaling Theory Using Automated Measurements of Vascular Networks. PLoS Comput Biol 11(8): e1004455. doi:10.1371/journal.pcbi.1004455](https://dx.doi.org/10.1371/journal.pcbi.1004455)

## Authors and community

Angicart was written by Mitchell Newberry <mitchell@silverninja.net> and is (c)
Mitchell Newberry 2011-2015.  Bug reports, comments, and feature requests are
welcome.
