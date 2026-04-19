# cs3110project

CS3110 Group Project for The Druid Cows

## Members

1. Rohan Anne (ra733)
2. Eric Shen (ezs22)
3. Charlie Xue (cx267)
4. Saiakhil Chilaka (sc3484)
5. Eric Sun (es2329)

## Installation

install sdl2 for your platform.
- macos: `brew install sdl2`
- windows: install SDL2 dev libraries via vcpkg or manually
- linux: install sdl2 with your package manager, e.g. `sudo apt install -y libsdl2-dev` or `pacman -Syu sdl2`

then run `opam install . --deps-only` to install the project dependencies

## Building

run `dune build`

## Running

run`dune exec ocaml-voxel`
