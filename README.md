# typerex-attic

A set of simple tools and libraries that we developed over the years
for temporary OCaml projects. Most of them are only
maintained/improved when we need them for a task.

# Installation

## Dependencies

The following dependencies are needed:
 * ocp-build >= 1.99.10-beta
   Needed for:
   * ocp-build: build tool
   * ocp-pp: preprocessor
   * ocplib-compat: compatibility between String and Bytes
   * ocplib-lang & ocplib-system: various standard functions
 * OCaml >= 4.00

# OCaml Tools

* ocp-check-globals: display global mutable values stored in the modules
    of an application.
* ocp-check-poly: display the use of polymorphic functions, that can
    sometimes be dangerous (polymorphic comparisons, etc.)
* ocp-manager: wrappers around OCaml binaries to automatically choose
    the correct opam switch.
* ocp-check-crcs: check the consistency of binary files in an OCaml
    distribution.
* ocp-check-headers: print the headers of all files in a project, can be
    used to add/replace headers.
* ocp-imports: print values imported by a module. Can also display imports
    by set of modules, to get the architecture of a project.
* ocp-pack: pack several OCaml source files into one source file, to get
   a result similar to the one of -pack 
