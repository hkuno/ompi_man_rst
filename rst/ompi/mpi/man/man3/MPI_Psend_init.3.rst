.. _mpi_psend_init:

MPI_Psend_init
==============
.. include_body

:ref:`MPI_Psend_init` - Initializes a partitioned send.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c
   :linenos:

   #include <mpi.h>
   int MPI_Psend_init(const void *buf, int partitions, int count, MPI_Datatype datatype, int dest,
   	int tag, MPI_Comm comm, MPI_Request *request)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran
   :linenos:

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PSEND_INIT(BUF, PARTITIONS, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)
   	<type>	BUF(*)
   	INTEGER	PARTITIONS, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran
   :linenos:

   USE mpi_f08
   MPI_Psend_init(buf, partitions, count, datatype, dest, tag, comm, request, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
   	INTEGER, INTENT(IN) :: partitions, count, dest, tag
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----- ----------

* ``buf``: Initial address of send buffer (choice). 

* ``partitions``: Number of partitions (integer). 

* ``count``: Number of elements to be sent per partition (integer). 

* ``datatype``: Datatype of each element (handle). 

* ``dest``: Rank of source (integer). 

* ``tag``: Message tag (integer). 

* ``comm``: Communicator (handle). 

OUTPUT PARAMETERS
------ ----------

* ``request``: Communication request (handle). 

* ``IERROR``: Fortran only: Error status (integer). 

ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

NOTE
----

The current implementation is an early prototype and is not fully
compliant with the MPI-4.0 specification. Specifically this function and
it's counterpart (MPI_Precv_init) will block until the partitioned
communication request is initialized on both ends. This behavior will be
corrected in future versions.


.. seealso:: | :ref:`MPI_Precv_init` 
