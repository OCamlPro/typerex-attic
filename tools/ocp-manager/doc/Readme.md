# ocp-manager

ocp-manager is a tool to manage several versions of OCaml on the same
computer.

## Compilation and Installation

You need 'ocp-build' installed on your computer:

    opam install ocp-build

Now, you can compile `ocp-manager`:

    ./configure
    make
    make install

You need to configure your PATH variable. You can add in your ~/bashrc (or
  whatever configure file for your shell):

    eval `ocp-manager -config`

## How to update

If you have a version of 'ocp-manager' before Feb 13. 2014, you should
restore what it might have changed:

   ocp-manager -restore

## Usage

### Managing Switches

Switches are directories containing OCaml binaries. For example, every OPAM
switch is an ocp-manager switch. The distribution switch in /usr/bin is
known as `distrib` in ocp-manager.

You can list available switches using:

    ocp-manager -list

You can set the global switch to `SWITCH` using:

    ocp-manager -set SWITCH

You can also set `OCAML_VERSION` in your terminal to just change the
version of OCaml in a terminal or for a command:

    OCAML_VERSION=opam:4.01.0 ocamlc -v

### Managing Tools

You can see which tools in the current switch are managed by 'ocp-manager':

    ocp-manager -tools

ocp-manager has a default list of tools. If some tool CMD is not managed by
ocp-manager, you can use:

    ocp-manager -add-tool CMD

You can also add all tools from the current switch:

    ocp-manager -add-all

Of course, you can also stop ocp-manager from managing a tool CMD:

    ocp-manager -remove-tool CMD

### Managing Default Tools

If a tool is not available in the current switch, ocp-manager is going
to check if a default tool is available. You can define your own default
tools. For example, for `ocp-indent`:

    ocp-manager -add-default /usr/local/bin/ocp-indent

`/usr/local/bin/ocp-indent` will now be used in all switches where
`ocp-indent is not locally available.

You can also directly copy the executable:

    cp /usr/local/bin/ocp-indent ~/.ocp/manager-defaults/

### Compiling OCaml to a new Switch

To add a new version in the list, you just need to install a version of OCaml
in $HOME/.ocp/manager-switches/ocaml-$VERSION/{bin,lib,man}. You can use:

    ocp-manager -compile SOME-VERSION
 
in the sources of OCaml to compile and install a new version of OCaml,
called SOME-VERSION in the list.


## OPAM switches

`ocp-manager` can manage OPAM switches. Such switches should be
prefixed with "opam:". For example, you can use "opam:3.12.1" for OPAM
switch "3.12.1".

Be careful never to use `opam config env`, as this command will
override the PATH variable and prevent `ocp-manager` from working.

## Per-directory configuration

With `ocp-manager`, you can define a per-project switch: at the root
of your project, create a file '.ocp-switch' containing the switch
name, or '.opam-switch' containing the OPAM switch name.

For example, in a project `typerex`, I can have `.ocp-switch`
containing `opam:4.01.0`, or `.opam-switch` containing `4.01.0`, so
that, whatever the configuration of OPAM and `ocp-manager`, I will
always use 4.01.0 to compile this project.



