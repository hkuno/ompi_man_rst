.. _open-mpi:

Open-MPI
====-===

.. include_body

Open MPI - General information

OPEN MPI
--------

Open MPI is an open source implementation of MPI (message-passing
interface), the industry-standard specification for writing
message-passing programs. Message passing is a programming model that
gives the programmer explicit control over interprocess communication.

The MPI specification was developed by the MPI Forum, a group of
software developers, computer vendors, academics, and computer-science
researchers whose goal was to develop a standard for writing
message-passing programs that would be efficient, flexible, and
portable.

The outcome, known as the MPI Standard, was first published in 1993; its
most recent version (MPI-3.1) was published in June 2015. Open MPI
includes all MPI 3.1-compliant routines.

For more information about Open MPI, see https://www.open-mpi.org.

The MPI standards are available at https://www.mpi-forum.org.

MAN PAGE SYNTAX
---------------

Man pages for Open MPI and Open MPI I/O routines are named according to
C syntax, that is, they begin with the prefix ``MPI_``, all in
uppercase, and the first letter following the ``MPI_`` prefix is also
uppercase. The rest of the letters in the routine are all lowercase, for
example, :ref:`MPI_Comm_get_attr`.

ENVIRONMENT
-----------

To fine-tune your Open MPI environment, you can either use arguments to
the ``mpirun`` or ``mpiexec`` commands, or you can use MCA parameters.

For more information on arguments, see the ``mpirun``\ (1) man page.

For a complete listing of MCA parameters and their descriptions, issue
the command ``ompi_info --all``. This information also appears in the
FAQ on the Open MPI web site at
https://www.open-mpi.org/faq/?category=tuning#mca-params.

ERRORS
------

All MPI routines (except :ref:`MPI_Wtime` and :ref:`MPI_Wtick`) return an
error value; C routines as the value of the function and Fortran
routines in the last argument. Before the value is returned, the current
MPI error handler is called. By default, this error handler aborts the
MPI job. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned.
Note that MPI does not guarantee that an MPI program can continue past
an error.

For more information on Open MPI error codes, see ``mpi.h`` in the
``include`` directory.

Standard error return classes for Open MPI:

\|--------------------------|-------------|--------------------------------\|
\| Error name \| Error value \| Description \|
\|--------------------------|-------------|--------------------------------\|
\| MPI_SUCCESS \| 0 \| Successful return code. \| \| MPI_ERR_BUFFER \| 1
\| Invalid buffer pointer. \| \| MPI_ERR_COUNT \| 2 \| Invalid count
argument. \| \| MPI_ERR_TYPE \| 3 \| Invalid datatype \| \| MPI_ERR_TAG
\| 4 \| Invalid tag argument. \| \| MPI_ERR_COMM \| 5 \| Invalid
communicator. \| \| MPI_ERR_RANK \| 6 \| Invalid rank. \| \|
MPI_ERR_REQUEST \| 7 \| Invalid MPI_Request \| \| MPI_ERR_ROOT \| 8 \|
Invalid root. \| \| MPI_ERR_GROUP \| 9 \| Null group passed to \| \|
MPI_ERR_OP \| 10 \| Invalid operation. \| \| MPI_ERR_TOPOLOGY \| 11 \|
Invalid topology. \| \| MPI_ERR_DIMS \| 12 \| Illegal dimension \| \|
MPI_ERR_ARG \| 13 \| Invalid argument. \| \| MPI_ERR_UNKNOWN \| 14 \|
Unknown error. \| \| MPI_ERR_TRUNCATE \| 15 \| Message truncated on \|
\| MPI_ERR_OTHER \| 16 \| Other error; use \| \| \| \| Error_string. \|
\| MPI_ERR_INTERN \| 17 \| Internal error code. \| \| MPI_ERR_IN_STATUS
\| 18 \| Look in status for error \| \| MPI_ERR_PENDING \| 19 \| Pending
request. \| \| MPI_ERR_ACCESS \| 20 \| Permission denied. \| \|
MPI_ERR_AMODE \| 21 \| Unsupported amode passed \| \| MPI_ERR_ASSERT \|
22 \| Invalid assert. \| \| MPI_ERR_BAD_FILE \| 23 \| Invalid file name
(for \| \| MPI_ERR_BASE \| 24 \| Invalid base. \| \| MPI_ERR_CONVERSION
\| 25 \| An error occurred in a \| \| MPI_ERR_DISP \| 26 \| Invalid
displacement. \| \| MPI_ERR_DUP_DATAREP \| 27 \| Conversion functions \|
\| \| \| :ref::ref:`MPI_REGISTER_DATAREP`. \| \| MPI_ERR_FILE_EXISTS \| 28
\| File exists. \| \| MPI_ERR_FILE_IN_USE \| 29 \| File operation could
not \| \| MPI_ERR_FILE \| 30 \| Invalid file handle. \| \|
MPI_ERR_INFO_KEY \| 31 \| Illegal info key. \| \| MPI_ERR_INFO_NOKEY \|
32 \| No such key. \| \| MPI_ERR_INFO_VALUE \| 33 \| Illegal info value.
\| \| MPI_ERR_INFO \| 34 \| Invalid info object. \| \| MPI_ERR_IO \| 35
\| I/O error. \| \| MPI_ERR_KEYVAL \| 36 \| Illegal key value. \| \|
MPI_ERR_LOCKTYPE \| 37 \| Invalid locktype. \| \| MPI_ERR_NAME \| 38 \|
Name not found. \| \| MPI_ERR_NO_MEM \| 39 \| Memory exhausted. \| \|
MPI_ERR_NOT_SAME \| 40 \| Collective argument not \| \| MPI_ERR_NO_SPACE
\| 41 \| Not enough space. \| \| MPI_ERR_NO_SUCH_FILE \| 42 \| File (or
directory) does \| \| MPI_ERR_PORT \| 43 \| Invalid port. \| \|
MPI_ERR_PROC_ABORTED \| 74 \| Operation failed because \| \|
MPI_ERR_QUOTA \| 44 \| Quota exceeded. \| \| MPI_ERR_READ_ONLY \| 45 \|
Read-only file system. \| \| MPI_ERR_RMA_CONFLICT \| 46 \| Conflicting
accesses to \| \| MPI_ERR_RMA_SYNC \| 47 \| Erroneous RMA \| \|
MPI_ERR_SERVICE \| 48 \| Invalid \| \| MPI_ERR_SIZE \| 49 \| Invalid
size. \| \| MPI_ERR_SPAWN \| 50 \| Error spawning. \| \| MPI \| 51 \|
Unsupported datarep \| \| \_ERR_UNSUPPORTED_DATAREP \| \| passed to \|
\| \| \| :ref::ref:`MPI_File_set_view`. \| \| MPI_E \| 52 \| Unsupported
operation, \| \| RR_UNSUPPORTED_OPERATION \| \| such as seeking on a \|
\| MPI_ERR_WIN \| 53 \| Invalid window. \| \| MPI_T_ERR_MEMORY \| 54 \|
Out of memory. \| \| M \| 55 \| Interface not \| \|
PI_T_ERR_NOT_INITIALIZED \| \| initialized. \| \| MPI_T_ERR_CANNOT_INIT
\| 56 \| Interface not in the \| \| MPI_T_ERR_INVALID_INDEX \| 57 \| The
enumeration index is \| \| MPI_T_ERR_INVALID_ITEM \| 58 \| The item
index queried \| \| MPI_T_ERR_INVALID_HANDLE \| 59 \| The handle is
invalid. \| \| MPI_T_ERR_OUT_OF_HANDLES \| 60 \| No more handles \| \| M
\| 61 \| No more sessions \| \| PI_T_ERR_OUT_OF_SESSIONS \| \|
available. \| \| M \| 62 \| Session argument is not \| \|
PI_T_ERR_INVALID_SESSION \| \| a valid session. \| \| MP \| 63 \|
Variable cannot be set \| \| I_T_ERR_CVAR_SET_NOT_NOW \| \| at this
moment. \| \| MPI_T_ERR_CVAR_SET_NEVER \| 64 \| Variable cannot be set
\| \| MPI \| 65 \| Variable cannot be \| \| \_T_ERR_PVAR_NO_STARTSTOP \|
\| started or stopped. \| \| MPI_T_ERR_PVAR_NO_WRITE \| 66 \| Variable
cannot be \| \| MPI_T_ERR_PVAR_NO_ATOMIC \| 67 \| Variable cannot be
read \| \| MPI_ERR_RMA_RANGE \| 68 \| Target memory is not \| \| \| \|
:ref::ref:`MPI_WIN_CREATE_DYNAMIC`, \| \| MPI_ERR_RMA_ATTACH \| 69 \|
Memory cannot be \| \| MPI_ERR_RMA_FLAVOR \| 70 \| Passed window has the
\| \| MPI_ERR_RMA_SHARED \| 71 \| Memory cannot be shared \| \|
MPI_T_ERR_INVALID \| 72 \| Invalid use of the \| \|
MPI_T_ERR_INVALID_NAME \| 73 \| The variable or category \| \|
MPI_ERR_LASTCODE \| 93 \| Last error code. \|


.. seealso:: :ref:`MPI_T`
