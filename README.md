# MineCaml

CS3110 Group Project for The Druid Cows

## Installation

Refer to [INSTALL.md](INSTALL.md) for installation instructions.

## Building

```sh
dune build
```

## Running

```sh
dune exec ocaml-voxel
```

## Testing

normal testing: 
```sh
dune test
```

testing with coverage (files located in `_coverage`):

```sh
dune test --instrument-with bisect_ppx --force
bisect-ppx-report html
```

