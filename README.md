# YSC2229: Part Two Libraries

This project contains the libraries with the implementations of
algorithms and data structures for the last seven weeks of Introductory
Data Structures and Algorithms (YSC2229). The lecture notes for the
course are available at

https://ilyasergey.net/YSC2229

## Project Structure 

The project only contains libraries, which are all are defined in
the folder `lib`.

To build everything that is meant to be installed in this project,
type:

## Building the Project

Building the project requires OCaml version 4.06.1 or higher. You
install required opam packages (specifically, the build tool `dune`)
using the commands listed below:

```
opam install -y dune
```

Next, to build the project, simply type:

```
make
```

## Using the Project as a Library

Once successfully built, you can install the contents of the Week 8-14
of YSC2229 as libraries to use in your own projects. Check out the
example project explaining how to use the libraries from Weeks 1-6:

https://github.com/ilyasergey/ysc2229-examples

### Installing the project via opam

To install the project as an independent packege, from the root folder
of the project, run

```
opam install .
```

If in the future you need to update the package installation, you can
reinstall it as follows (again, from the root folder of the project):

```
opam reinstall .
```

To uninstall the package, type in terminal

```
opam uninstall ysc2229-part-one
```

## Running with utop

From the console, run

```
dune utop . --profile release
```

See the
[part-one libraries](https://github.com/ilyasergey/ysc2229-examples)
and
[complementary project](https://github.com/ilyasergey/ysc2229-examples)
with examples on how to the libraries in your development.

## Submitting changes to the project

You can fork the project on GitHub for experimentation.

If you don't like certain functionality and know how to fix it, you
can also file Pull Requests to the master repository via GitHub
interface.
