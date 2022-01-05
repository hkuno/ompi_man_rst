.. _shmem_barrier_all:

shmem_barrier_all
=================
.. include_body

shmem_barrier_all - Suspends the execution of the calling PE until all
other PEs issue a call to this particular shmem_barrier_all() statement.

SYNOPSIS
--------

C or C++:

.. code-block:: c++
   :linenos:

   #include <mpp/shmem.h>

   void shmem_barrier_all(void);

Fortran:

.. code-block:: fortran
   :linenos:

   include 'mpp/shmem.h'

   CALL SHMEM_BARRIER_ALL

DESCRIPTION
-----------

The shmem_barrier_all routine does not return until all other PEs have
entered this routine at the same point of the execution path.

Prior to synchronizing with other PEs, shmem_barrier_all ensures
completion of all previously issued local memory stores and remote
memory updates issued via SHMEM functions such as *shmem_put32*\ (3).

EXAMPLES
--------

::

   setup_data()
   {
     if (shmem_my_pe() == 0) {
       setup();
     }

     /* All PEs wait for PE 0 to complete setup().  */
     shmem_barrier_all();
   }


.. seealso:: *:ref:`shmem_barrier` \ (3), *:ref:`shmem_init` \ (3)
