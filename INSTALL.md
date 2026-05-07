# Installation Instructions

You need to install the OCaml dependencies and SDL2 to run this project.

## OCaml

Install OCaml, OPAM, and Dune for your platform.

## SDL2

You also need to install SDL2.

- macos: `brew install sdl2`
- windows: install SDL2 dev libraries via vcpkg or manually
- linux: install SDL2 with your package manager, e.g. `sudo apt install -y libsdl2-dev` or `pacman -Syu sdl2`

## Project Dependencies

Then, run `opam install . --deps-only` to install the project dependencies.

## Building

```sh
dune build
```

## Running

```sh
dune exec ocaml-voxel
```
