# Silq

Silq is a high-level programming language for quantum computing with a strong static type system.
More information: https://silq.ethz.ch

## Branch - Verify (In Development)
The approach of this branch is to convert the AST of a Silq file into a JSON file. This JSON can then be used by other programming languages to perform analysis (such as the verif-silq submodule). This branch and the respective submodule are in development and could be **unstable**.

## Installation
To install via vscode without building from source, see: https://silq.ethz.ch/install

## Build Instructions

### GNU/Linux and OSX

#### Quick build

1. Run `dependencies-release.sh` to download the LDC D compiler into the local directory.

2. Run `build-release.sh` to build Silq.

##### Additional information

Silq is written in the D programming language. D compilers are available at http://dlang.org/download.html.

### FreeBSD

Silq can be built from a port or installed as a binary package on FreeBSD/amd64. 
Other FreeBSD platforms do currently lack the D language support required by silq.

To install from source, assuming an up-to-date ports tree is available:
```
$ cd /usr/ports/lang/silq
$ make install
```
(Or use portmaster or any of the other ports management tools available in FreeBSD ...)

To install from a package:
```
$ pkg install silq
```

### Other platforms

The build instructions given here are for GNU/Linux and OSX. Silq can also be built on other platforms.
Feel free to write a pull request with working build scripts for your favourite platform.

### Example

```
$ ./dependencies-release.sh && ./build-release.sh
```

## Using Silq

Run `./silq example.slq`, where `example.slq` is a Silq source file to type check that source file.

Run `./silq example.slq --run`, where `example.slq` is a Silq source file to type check and simulate the main function in that source file.

### Additional command-line options

Run `./silq --help` to display information about supported command-line options.
