ocp-imports
===========

A tool to display values and types imported by a unit (using .cmt information).

Compile your project
--------------------

`ocp-imports` requires `.cmt` files to work. Thus, you need to compile
your project using `-bin-annot` to generate `.cmt` files. With
`ocp-build`, this is simply done by adding `comp += [ "-bin-annot" ]`
in the `.ocp` file at the root of your project.

Display all imports
-------------------

The basic usage of `ocp-imports` is to print all the values and types
used by some `.cmt` files:

Suppose you have a file `toto.ml`:
{{{
let f x = x + x
let () = Printf.eprintf "f(4) = %d\n%!" (f 4)
}}}

Then, you can run:
{{{
$ ocamlc -c -bin-annot toto.ml
$ ocp-imports.asm toto.cmt 
Module: Toto
  Values:
   Pervasives.+
   Printf.eprintf
  Types:
   Pervasives.format
}}}

If you provide several `.cmt` files, all of them will be displayed. As
a special features, you can use `-ocp PACKAGE` if you use `ocp-build` to
select all the `.cmt` files in the `_obuild` directory for this package.

You can use the arguments `-no-values` or `no-types` to choose to
display only one category of imports.

You can use some arguments to select or black-list modules whose
values/types you don't want to see in the imports:
* `-no-stdlib` to black-list imports from the standard library
* `-only-cross` to print only imports coming from the same set of modules as
   provided in the `.cmt` list
* `-from-rest MODULES` to print only imports from these modules (or .cmt files)
* `-not-from-rest MODULES` to avoid printing imports from all these modules

Display architecture (`-arch` argument)
--------------------

`ocp-imports` has a special mode to print references between modules,
organized as sets, sometimes easier to use to see the general
architecture of a project. For that, you can use the `-arch` argument.

All modules should belong to a set. For that, you can use:
* `-obuild`: this will recurse the `_obuild` directory of `ocp-build`,
    collecting both `.cmt` files and the name of the package to which
    they belong.
* `-packages FILE`: this will provide a file in which sets of modules are
    provided.

Here is an example of such a file:
{{{
stdlib: arg.cmi format.cmi moreLabels.cmi set.cmi array.cmi gc.cmi
   nativeint.cmi sort.cmi arrayLabels.cmi genlex.cmi obj.cmi
compiler-libs: annot.cmi arch.cmi asmgen.cmi asmlibrarian.cmi
  asmlink.cmi asmpackager.cmi ast_mapper.cmi asttypes.cmi btype.cmi
  bytegen.cmi
bigarray: bigarray
threads: thread mutex condition
dynlink: Dynlink
cmdliner: cmdliner
js_of_ocaml: compiler.cmi cSS.cmi dom.cmi dom_events.cmi dom_html.cmi
# file.cmi (removed because it conflicts with ocplib-system:file)
}}}

You can also use the `opam-sets.txt` in this directory.
