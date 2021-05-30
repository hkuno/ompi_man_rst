.. _MPI_Type_extent:

MPI_Type_extent
~~~~~~~~~~~~~~~

:ref:`MPI_Type_extent`  - Returns the extent of a data type, the difference
between the upper and lower bounds of the data type -- use of this
routine is deprecated.

SYNTAX
======

C Syntax
--------

.. code-block:: c
   :linenos:

   #include <mpi.h>
   int MPI_Type_extent(MPI_Datatype datatype, MPI_Aint *extent)

Fortran Syntax
--------------

.. code-block:: fortran
   :linenos:

   INCLUDE 'mpif.h'
   MPI_TYPE_EXTENT(DATATYPE, EXTENT, IERROR)
   	INTEGER	DATATYPE, EXTENT, IERROR

INPUT PARAMETER
===============

* ``datatype``: Datatype (handle). 

OUTPUT PARAMETERS
=================

* ``extent``: Datatype extent (integer). 

* ``IERROR``: Fortran only: Error status (integer). 

DESCRIPTION
===========

Note that use of this routine is *deprecated* as of MPI-2. Please use
:ref:`MPI_Type_get_extent`  instead.

:ref:`MPI_Type_extent`  returns the extent of a data type, the difference
between the upper and lower bounds of the data type.

In general, if

::

       Typemap = {(type(0), disp(0)), ..., (type(n-1), disp(n-1))}

then the lower bound of Typemap is defined to be

::

                 ( min(j) disp(j)                         if no entry has
     lb(Typemap)=(                                        basic type lb
                 (min(j) {disp(j) such that type(j) = lb} otherwise

Similarly, the upper bound of Typemap is defined to be

::

                 (max(j) disp(j) + sizeof(type(j)) + e    if no entry has
     ub(Typemap)=(                                        basic type ub
                 (max(j) {disp(j) such that type(j) = ub} otherwise

Then

::

       extent(Typemap) = ub(Typemap) - lb(Typemap)

If type(i) requires alignment to a byte address that is a multiple of
k(i), then e is the least nonnegative increment needed to round
extent(Typemap) to the next multiple of max(i) k(i).

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


.. seealso:: | :ref:`MPI_Type_get_extent` 
