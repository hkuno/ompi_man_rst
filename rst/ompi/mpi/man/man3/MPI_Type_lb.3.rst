.. _MPI_Type_lb:

MPI_Type_lb
~~~~~~~~~~~
:ref:`MPI_Type_lb`  - Returns the lower bound of a data type -- use of this
routine is deprecated.

SYNTAX
======

C Syntax
--------

.. code-block:: c
   :linenos:

   #include <mpi.h>
   int MPI_Type_lb(MPI_Datatype datatype, MPI_Aint *displacement)

Fortran Syntax
--------------

.. code-block:: fortran
   :linenos:

   INCLUDE 'mpif.h'
   MPI_TYPE_LB(DATATYPE, DISPLACEMENT, IERROR)
   	INTEGER	DATATYPE, DISPLACEMENT, IERROR

INPUT PARAMETER
===============

* ``datatype``: Datatype (handle). 

OUTPUT PARAMETERS
=================

* ``displacement``: Displacement of lower bound from origin, in bytes (integer). 

* ``IERROR``: Fortran only: Error status (integer). 

DESCRIPTION
===========

Note that use of this routine is *deprecated* as of MPI-2. Please use
:ref:`MPI_Type_get_extent`  instead.

:ref:`MPI_Type_lb`  returns the lower bound of a data type. This may differ from
zero if the type was constructed using :ref:`MPI_LB` .

The "pseudo-datatypes," :ref:`MPI_LB`  and :ref:`MPI_UB` , can be used, respectively, to
mark the lower bound (or the upper bound) of a datatype. These
pseudo-datatypes occupy no space (extent (:ref:`MPI_LB)`  = extent (:ref:`MPI_UB)`  =0.
They do not affect the size or count of a datatype, and do not affect
the context of a message created with this datatype. However, they do
affect the definition of the extent of a datatype and, therefore, affect
the outcome of a replication of this datatype by a datatype constructor.

In general, if

::

       Typemap = {(type0, disp0), ..., (type(n-1), disp(n-1)}

then the lower bound of Typemap is defined to be

::


                     (min(j) disp(j)                          if no entry has
       lb(Typemap) = (                                        basic type lb
                     (min(j) {disp(j) such that type(j) = lb} otherwise

Similarly, the upper bound of Typemap is defined to be

::


                     (max(j) disp(j) + sizeof((type(j)) + e   if no entry has
       ub(Typemap) = (                                        basic type ub
                     (max(j) {disp(j) such that type(j) = ub} otherwise

   Then

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
