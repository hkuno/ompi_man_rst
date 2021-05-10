.. _shmem_n_pes:

shmem_n_pes
~~~~~~~~~~~
num_pes, \_num_pes, shmem_n_pes - Returns the number of processing
elements (PEs) used to run the application.

SYNOPSIS
========

C or C++:

.. code-block:: c++
   :linenos:

   #include <mpp/shmem.h>
   int _num_pes (void);
   int shmem_n_pes (void);

Fortran:

.. code-block:: fortran
   :linenos:

   include 'mpp/shmem.fh'
   I = NUM_PES ()
   I = SHMEM_N_PES ()

DESCRIPTION
===========

num_pes() or shmem_n_pes() return the total number of PEs running in an
application.


.. seealso:: *intro_:ref:`shmem` \ (3), *:ref:`shmem_my_pe` \ (3), *:ref:`shmem_init` \ (3)
