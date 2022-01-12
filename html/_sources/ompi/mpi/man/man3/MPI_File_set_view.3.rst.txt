.. _mpi_file_set_view:


MPI_File_set_view
=================

.. include_body

:ref:`MPI_File_set_view` - Changes process's view of data in file
(collective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c
   :linenos:

   #include <mpi.h>
   int MPI_File_set_view(MPI_File fh, MPI_Offset disp,
   	MPI_Datatype etype, MPI_Datatype filetype,
   	const char *datarep, MPI_Info info)


Fortran Syntax (see FORTRAN 77 NOTES)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran
   :linenos:

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_SET_VIEW(FH, DISP, ETYPE,
   	FILETYPE, DATAREP, INFO, IERROR)
   	INTEGER	FH, ETYPE, FILETYPE, INFO, IERROR
   	CHARACTER*(*)	DATAREP
   	INTEGER(KIND=MPI_OFFSET_KIND)	DISP


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran
   :linenos:

   USE mpi_f08
   MPI_File_set_view(fh, disp, etype, filetype, datarep, info, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: disp
   	TYPE(MPI_Datatype), INTENT(IN) :: etype, filetype
   	CHARACTER(LEN=*), INTENT(IN) :: datarep
   	TYPE(MPI_Info), INTENT(IN) :: info
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
-----/----------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``disp``: Displacement (integer).
* ``etype``: Elementary data type (handle).
* ``filetype``: File type (handle). See Restrictions, below.
* ``datarep``: Data representation (string).
* ``info``: Info object (handle).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The :ref:`MPI_File_set_view` routine changes the process's view of the data in
the file -- the beginning of the data accessible in the file through
that view is set to *disp;* the type of data is set to *etype;* and the
distribution of data to processes is set to *filetype.* In addition,
:ref:`MPI_File_set_view` resets the independent file pointers and the shared
file pointer to zero. :ref:`MPI_File_set_view` is collective across the *fh*;
all processes in the group must pass identical values for *datarep* and
provide an *etype* with an identical extent. The values for *disp*,
*filetype*, and *info* may vary. It is erroneous to use the shared file
pointer data-access routines unless identical values for *disp* and
*filetype* are also given. The data types passed in *etype* and
*filetype* must be committed.

The *disp* displacement argument specifies the position (absolute offset
in bytes from the beginning of the file) where the view begins.

The :ref:`MPI_File_set_view` interface allows the user to pass a
data-representation string to MPI I/O via the *datarep* argument. To
obtain the default value pass the value "native". The user can also pass
information via the *info* argument. See the HINTS section for a list of
hints that can be set. For more information, see the MPI-2 standard.


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


FORTRAN 77 NOTES
----------------

The MPI standard prescribes portable Fortran syntax for the *DISP*
argument only for Fortran 90. FORTRAN 77 users may use the non-portable
syntax

::

        INTEGER*MPI_OFFSET_KIND DISP

where MPI_OFFSET_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.


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
