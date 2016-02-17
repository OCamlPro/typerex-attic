
                             ocp-check-crcs
                             ==============

The goal of this tool is to check the consistency of an OCaml installation,
sometimes together with a user project.
 It could do the following checks (TODO list):
 * Detect that a given module cannot be used because one of its dependencies
    is not available in the repository;
 * Detect that a library cannot be used because two of its dependencies are
    in conflict;
 * Detect conflicting libraries that define the same internal module;
 * Detect that the only .cmi provided does not implement the interface
     expected by the implementations.
 * Detect .cmi files without implementations

Usage
=====

ocp-check-crcs DIRS

  Scan the directories DIRS and check consistency of installation

ocp-check-crcs -save FILE DIRS

  Scan the directories DIRS, save the graph of dependencies in FILE,
  and check consistency of installation.

ocp-check-crcs -load FILE

  Load the graph of dependencies from FILE, and check consistency of
  installation.

ocp-check-crcs {-load FILE | DIRS } -query MODNAME

  Print information on module MODNAME (which files implements or
   use this module/interface)

ocp-check-crcs -man > ocp-check-crcs.1; man ./ocp-check-crcs.1

  To generate and read the manpage




