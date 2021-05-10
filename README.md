The rst files in this directory were created from the ompi man pages using
the scripts from https://github.com/hkuno/ompi_man_rst_scripts

The commit for the source ompi man pages used to build the rst files was:
   commit 46948d2a4003099175b594a8aea710b3834e2815
   Author: Jeff Squyres <jsquyres@cisco.com>
   Date:   Sat May 1 18:16:57 2021 -0400

       squashme WIP checkpoint -- bring in README.md changes from master
    
       Signed-off-by: Jeff Squyres <jsquyres@cisco.com>

Use sphinx-build to convert these rst files to man pages and html pages:
   *  sphinx-build -b html -c rst rst/ html/ 
   *  sphinx-build -b man  -c rst rst/ man/ 
