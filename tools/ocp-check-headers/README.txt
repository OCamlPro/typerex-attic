ocp-check-headers
=================

A very simple tool to manage headers of a collection of source
files. Basically, it helps you:
(1) collect all the headers and the files to which they are applied,
(2) add a header to files without one,
(3) replace a header by another one

It is called with a list of directories or files. It then generates 3
reports called "headers-ml.txt", "headers-cc.txt" and "headers-sh.txt"
containing the headers and the files where they were found. Each
header has a uniq identifier, based on a checksum.

You can then use these identifiers to add or replace headers. Standard
headers can be stored in ~/.ocp/check-headers/headers{.ml, .cc, .sh} files.

Also, ocp-check-headers will read per-directory files:
.ocp-check-headers-ignore-files : files or extensions (starting with .) to
   ignore while scanning directories
.ocp-check-headers-ignore-headers: headers to ignore

Headers are supposed to start and end with the same beginning of line:

(************************** for OCaml
/************************** for C-likes
########################### for shells

