.. _mpijavac:

mpijavac
~~~~~~~~

mpijava -- Open MPI Java wrapper compiler

SYNTAX
======

mpijava [-showme|-showme:compile|-showme:link] ...

OPTIONS
=======

--showme
   This option comes in several different variants (see below). None of
   the variants invokes the underlying compiler; they all provide
   information on how the underlying compiler would have been invoked
   had *--showme* not been used. The basic *--showme* option outputs the
   command line that would be executed to compile the program. **NOTE:**
   If a non-filename argument is passed on the command line, the
   *-showme* option will *not* display any additional flags. For
   example, both "mpijava --showme" and "mpijava --showme
   my_source.java" will show all the wrapper-supplied flags. But
   "mpijava --showme -v" will only show the underlying compiler name and
   "-v".

--showme:compile
   Output the compiler flags that would have been supplied to the java
   compiler.

--showme:link
   Output the linker flags that would have been supplied to the java
   compiler.

--showme:command
   Outputs the underlying java compiler command (which may be one or
   more tokens).

--showme:incdirs
   Outputs a space-delimited (but otherwise undecorated) list of
   directories that the wrapper compiler would have provided to the
   underlying java compiler to indicate where relevant header files are
   located.

--showme:libdirs
   Outputs a space-delimited (but otherwise undecorated) list of
   directories that the wrapper compiler would have provided to the
   underlying linker to indicate where relevant libraries are located.

--showme:libs
   Outputs a space-delimited (but otherwise undecorated) list of library
   names that the wrapper compiler would have used to link an
   application. For example: "mpi open-rte open-pal util".

--showme:version
   Outputs the version number of Open MPI.

See the man page for your underlying java compiler for other options
that can be passed through mpijava.

DESCRIPTION
===========

Conceptually, the role of these commands is quite simple: transparently
add relevant compiler and linker flags to the user's command line that
are necessary to compile / link Open MPI programs, and then invoke the
underlying compiler to actually perform the command.

As such, these commands are frequently referred to as "wrapper"
compilers because they do not actually compile or link applications
themselves; they only add in command line flags and invoke the back-end
compiler.

Overview
--------

*mpijava* is a convenience wrapper for the underlying java compiler.
Translation of an Open MPI program requires the linkage of the Open
MPI-specific libraries which may not reside in one of the standard
search directories of ld(1). It also often requires the inclusion of
header files what may also not be found in a standard location.

*mpijava* passes its arguments to the underlying java compiler along
with the -I, -L and -l options required by Open MPI programs.

The Open MPI Team *strongly* encourages using the wrapper compilers
instead of attempting to link to the Open MPI libraries manually. This
allows the specific implementation of Open MPI to change without forcing
changes to linker directives in users' Makefiles. Indeed, the specific
set of flags and libraries used by the wrapper compilers depends on how
Open MPI was configured and built; the values can change between
different installations of the same version of Open MPI.

Indeed, since the wrappers are simply thin shells on top of an
underlying compiler, there are very, very few compelling reasons *not*
to use *mpijava*. When it is not possible to use the wrappers directly,
the *-showme:compile* and *-showme:link* options should be used to
determine what flags the wrappers would have used.

NOTES
=====

It is possible to make the wrapper compilers multi-lib aware. That is,
the libraries and includes specified may differ based on the compiler
flags specified (for example, with the GNU compilers on Linux, a
different library path may be used if -m32 is seen versus -m64 being
seen). This is not the default behavior in a standard build, but can be
activated (for example, in a binary package providing both 32 and 64 bit
support). More information can be found at:

https://svn.open-mpi.org/trac/ompi/wiki/compilerwrapper3264

FILES
=====

The string that the wrapper compilers insert into the command line
before invoking the underlying compiler are stored in a text file
created by Open MPI and installed to
*$pkgdata/mpijava-wrapper-data.txt*, where *$pkgdata* is typically
*$prefix/share/openmpi*, and *$prefix* is the top installation directory
of Open MPI.

It is rarely necessary to edit this file, but it can be examined to gain
insight into what flags the wrappers are placing on the command line.

ENVIRONMENT VARIABLES
=====================

By default, the wrappers use the compilers that were selected when Open
MPI was configured. These compilers were either found automatically by
Open MPI's "configure" script, or were selected by the user in the CC,
CXX, F77, JAVAC, and/or FC environment variables before "configure" was
invoked. Additionally, other arguments specific to the compiler may have
been selected by configure.
