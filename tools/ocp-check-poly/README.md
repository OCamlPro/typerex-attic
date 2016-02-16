# ocp-check-poly

ocp-check-poly [OPTIONS] cmt_files

This tool finds all the applications of some predefined polymorphic
functions, and display their occurrences with their types.

It is usually used for "dangerous" polymorphic functions, such as
Pervasives.=, that checks for structural equality between two
values. When changing the types of these values, it might be necessary
to implement a specific comparison function, in which case it is
necessary to find all the occurrences of polymorphic comparisons that
need to be replaced.

## Options

### Specifing suspect functions

By default, ocp-check-poly assumes that the -stdlib option has been
provided, unless one or more of the following options is provided :

- -stdlib : search for all predefined suspect functions of Pervasives, List and Hashtbl.
- -Pervasives : search for all predefined suspect functions of Pervasives
  Currently, this set includes:
   - Pervasives.<
   - Pervasives.>
   - Pervasives.=
   - Pervasives.compare
- -List : search for all predefined suspect functions of List
  Currently, this set includes:  
   - List.mem
   - List.mem_assoc
   - List.remove_assoc
   - List.assoc
- -Hashtbl : search for all predefined suspect functions of Hashtbl
  Currently, this set includes:
   - Hashtbl.add
- -suspect PATH : add PATH as a suspect function, which occurrences should
  be displayed

### Specifying the printing behavior

- -merge-files : merge occurrences of the same suspect function in different
    files. By default, ocp-check-poly prints occurrences file by file.
- -one-line : print an occurrence and the location on the same line (useful
    when somebody wants to grep results). By default, ocp-check-poly prints
    first the occurrence name and type, and then all the locations on
    different lines, so that Emacs, for example, can navigate between them.
- -simple-types : print also occurrences with simple types. By default,
    ocp-check-poly removes occurrences where types are basic predefined types
    (int, char, string, int list, etc.). Such occurrences are usually not
    dangerous. This option, however, prevents this behavior, and displays
    all occurrences, even with simple types.

