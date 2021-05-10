.. _MPI_Start:

MPI_Start
~~~~~~~~~
:ref:`MPI_Start`  - Initiates a communication using a persistent request
handle.

SYNTAX
======

C Syntax
--------

.. code-block:: c
   :linenos:

   #include <mpi.h>
   int MPI_Start(MPI_Request *request)

Fortran Syntax
--------------

.. code-block:: fortran
   :linenos:

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_START(REQUEST, IERROR)
   	INTEGER	REQUEST, IERROR

Fortran 2008 Syntax
-------------------

.. code-block:: fortran
   :linenos:

   USE mpi_f08
   MPI_Start(request, ierror)
   	TYPE(MPI_Request), INTENT(INOUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETER
===============

* ``request``: Communication request (handle). 

OUTPUT PARAMETER
================

* ``IERROR``: Fortran only: Error status (integer). 

DESCRIPTION
===========

A communication (send or receive) that uses a persistent request is
initiated by the function :ref:`MPI_Start` .

The argument, request, is a handle returned by one of the persistent
communication-request initialization functions (:ref:`MPI_Send_init` ,
:ref:`MPI_Bsend_init` , :ref:`MPI_Ssend_init` , :ref:`MPI_Rsend_init` , :ref:`MPI_Recv_init)` . The
associated request should be inactive and becomes active once the call
is made.

If the request is for a send with ready mode, then a matching receive
should be posted before the call is made. From the time the call is made
until after the operation completes, the communication buffer should not
be accessed.

The call is local, with semantics similar to the nonblocking
communication operations (see Section 3.7 in the MPI-1 Standard,
"Nonblocking Communication.") That is, a call to :ref:`MPI_Start`  with a
request created by :ref:`MPI_Send_init`  starts a communication in the same
manner as a call to :ref:`MPI_Isend` ; a call to :ref:`MPI_Start`  with a request
created by :ref:`MPI_Bsend_init`  starts a communication in the same manner as a
call to :ref:`MPI_Ibsend` ; and so on.

ERRORS
======

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler` ; the predefined error handler :ref:`MPI_ERRORS_RETURN` 
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso:: | :ref:`MPI_Bsend_init` | :ref:`MPI_Rsend_init` | :ref:`MPI_Send_init` | :ref:`MPI_Sssend_init` | :ref:`MPI_Recv_init` | :ref:`MPI_Startall` 
