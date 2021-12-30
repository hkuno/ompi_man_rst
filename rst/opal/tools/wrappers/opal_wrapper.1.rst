.. _opal_wrapper:

opal_wrapper
~~~~~~~~~~~~

opal_wrapper - Back-end Open MPI wrapper command

SYNOPSIS
========

**opal_wrapper [options]**

DESCRIPTION
===========

**opal_wrapper** is not meant to be called directly by end users. It is
automatically invoked as the back-end by the Open MPI wrapper commands
such as: **mpicc**, **mpiCC**, **mpic++**, and **mpifort** (and its
legacy/deprecated names **mpif77** and **mpif90**).

Some Open MPI installations may have additional wrapper commands, and/or
have renamed the wrapper compilers listed above to avoid executable name
conflicts with other MPI implementations. Hence, you may also have
wrapper compilers installed including the following names:
**mpifort.openmpi** (and the legacy/deprecated names **mpif90.openmpi**
and **mpif77.openmpi**), **mpicxx.openmpi**, **mpiCC.openmpi**,
**mpicc.openmpi**, **mpic++.openmpi**, **opalcc**, **opalc++**,
**ortecc**, and **ortec++**,


.. seealso:: The following may exist depending on your particular Open MPIinstallation: **:ref:`mpicc` \ (1), **:ref:`mpiCC` \ (1), **:ref:`mpic++` \ (1),**:ref:`mpifort` \ (1), **:ref:`mpifort.openmpi` \ (1), **:ref:`mpicxx.openmpi` \ (1),**:ref:`mpiCC.openmpi` \ (1), **:ref:`mpicc.openmpi` \ (1), **:ref:`mpic++.openmpi` \ (1),**ortecc**\ (1), **ortec++**\ (1), **opalccc**\ (1), and the website at*https://www.open-mpi.org/*.AUTHORS=======The Open MPI maintainers -- see *https://www.open-mpi.org/* or the file*AUTHORS*.This manual page was originally contributed by Dirk Eddelbuettel<edd@debian.org>, one of the Debian GNU/Linux maintainers for Open MPI,and may be used by others.
