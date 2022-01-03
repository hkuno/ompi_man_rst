The rst files in this directory were created from the ompi man pages using
the scripts from https://github.com/hkuno/ompi_man_rst_scripts

The commit for the source ompi man pages used to build the rst files was:
    commit ecdd746337a8024b09295d2b76dc1773dadabfb4
    Merge: 6a6ff56c26 c79291ea86
    Author: Jeff Squyres <jsquyres@cisco.com>
    Date:   Tue Dec 28 13:43:18 2021 -0500
    
        Merge pull request #9793 from jsquyres/pr/fix-macos-build
        
        sharedfp: use opal_basename()

Use sphinx-build to convert these rst files to man pages and html pages:
   *  sphinx-build -b html -c rst rst/ html/ 
   *  sphinx-build -b man  -c rst rst/ man/ 
