AC_INIT(typerex-attic,1.0.3)
CONFIGURE_ARGS=$*

AC_COPYRIGHT(Copyright 2016 OCamlPro SAS)
AC_CONFIG_MACRO_DIR([m4])


AC_ARG_WITH([typerex],[], [  
  OBUILD_ROOTDIR=$withval
 ], [
  OBUILD_ROOTDIR=.
  ])
OBUILD_DSTDIR=$OBUILD_ROOTDIR/_obuild
AC_SUBST(OBUILD_ROOTDIR)
AC_SUBST(OBUILD_DSTDIR)
  
AC_ARG_ENABLE(version-check,
 [  --disable-version-check            do not check OCaml version],
 [VERSION_CHECK="$enableval"],
 [VERSION_CHECK="yes"])

AC_PROG_CC

# Check that OCaml version is greater or equal to 4.00.0

AC_PROG_OCAML

AX_SPLIT_VERSION(OCAMLVERSION)
OCAML_MAJOR_VERSION=$AX_MAJOR_VERSION
OCAML_MINOR_VERSION=$AX_MINOR_VERSION
OCAML_POINT_VERSION=$AX_POINT_VERSION

if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

  MINIMAL_VERSION=4.00.0
  AX_COMPARE_VERSION( [$OCAMLVERSION], [lt], [${MINIMAL_VERSION}],
     AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported. You need ${MINIMAL_VERSION} at least.]))


ocamldir='${libdir}/ocaml'
metadir='${ocamldir}'

AC_PROG_FINDLIB
if test "${OCAMLFIND}" != "no"; then
  metadir="$(${OCAMLFIND} -printconf destdir)"
  echo "initializing metadir from ocamlfind config"
fi

if test "x${prefix}" = "xNONE"; then
  if test "x${OPAM_PREFIX}" != "x"; then
    echo inheriting OPAM settings from OPAM_PREFIX
    prefix="${OPAM_PREFIX}"
    ocamldir='${libdir}'
    metadir="${OPAM_PREFIX}/lib"
  else
    prefix="$(dirname ${OCAMLLIB})"
    base="$(basename ${prefix})"
    if test "x${base}" = "xlib"; then
      prefix="$(dirname ${prefix})"
    fi
  fi
fi

AC_ARG_WITH(ocamldir,
 [AS_HELP_STRING([--with-ocamldir=DIRNAME],
               [root storage of the OCaml distribution])],
 [ocamldir="${withval}"],
 [])

AC_ARG_WITH(metadir,
 [AS_HELP_STRING([--with-metadir=DIRNAME],
               [store meta files in DIRNAME])],
 [metadir="${withval}"],
 [])

AC_SUBST(ocamldir)
AC_SUBST(metadir)

AC_SUBST(OCAML_MAJOR_VERSION)
AC_SUBST(OCAML_MINOR_VERSION)
AC_SUBST(OCAML_POINT_VERSION)
AC_SUBST(CONFIGURE_ARGS)
AC_SUBST(VERSION)
AC_CONFIG_FILES(
  Makefile.config 
  ../config.ocp
)
AC_OUTPUT

echo
echo "Summary:"
echo "prefix=${prefix}"
echo "exec_prefix=${exec_prefix}"
echo "bindir=${bindir}"
echo "   (Executables will be installed there)"
echo "libdir=${libdir}"
echo "ocamldir=${ocamldir}"
echo "   (OCaml files will be installed there)"
echo "metadir=${metadir}"
echo "   (META files will be installed there)"
echo "datarootdir=${datarootdir}"
echo "mandir=${mandir}"
echo "   (Manual pages will be installed there)"
