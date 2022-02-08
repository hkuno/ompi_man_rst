.. _mpi_file_set_info:


MPI_File_set_info
=================

.. include_body

:ref:`MPI_File_set_info` - Sets new values for hints (collective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_set_info(MPI_File fh, MPI_Info info)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_SET_INFO(FH, INFO, IERROR)
   	INTEGER	FH, INFO, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_set_info(fh, info, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	TYPE(MPI_Info), INTENT(IN) :: info
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
-----/----------------
* ``fh``: File handle (handle).

INPUT PARAMETER
---------------
* ``info``: Info object (handle).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_set_info` is a collective routine that sets new values for the
hints of the file associated with *fh*. These hints are set for each
file, using the :ref:`MPI_File_open`, :ref:`MPI_File_delete`, :ref:`MPI_File_set_view`, and
:ref:`MPI_File_set_info` routines. The opaque *info* object, which allows you
to provide hints for optimization of your code, may be different on each
process, but some *info* entries are required to be the same on all
processes: In these cases, they must appear with the same value in each
process's info object. See the HINTS section for a list of hints that
can be set.


HINTS
-----

The following hints can be used as values for the *info* argument.

SETTABLE HINTS:

{INDENT}{curline}

{INDENT}{curline}

{INDENT}{curline}

{INDENT}{curline}

{INDENT}{curline}

{INDENT}{curline}

NOTE: A buffer size smaller than the distance (in bytes) in a UNIX file
between the first byte and the last byte of the access request causes
MPI I/O to iterate and perform multiple UNIX read() or write() calls. If
the request includes multiple noncontiguous chunks of data, and the
buffer size is greater than the size of those chunks, then the UNIX
read() or write() (made at the MPI I/O level) will access data not
requested by this process in order to reduce the total number of write()
calls made. If this is not desirable behavior, you should reduce this
buffer size to equal the size of the contiguous chunks within the
aggregate request.

{INDENT}{curline}

{INDENT}{curline}

NON-SETTABLE HINTS:

{INDENT}{curline}


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
MPI_ERRORS_RETURN. The error handler may be changed with
:ref:`MPI_File_set_errhandler`; the predefined error handler
MPI_ERRORS_ARE_FATAL may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
