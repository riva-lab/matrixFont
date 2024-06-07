Automation scripts
==================

    This catalog contains automation scripts (Windows only).
    Actually working is those which name started with underscore "_".
    Other is executed from those mentioned above.
    Do NOT execute them unless you understand how it works.

    The "installer" subcatalog contains Inno Setup Scripts for
    building installation executables.

    The "tools" subdirectory contains additional tool executables
    needed for automation scripts.


Description of scripts
======================


_build-lazproject.bat
---------------------
    Script builds Lazarus project in all build modes.


_build-release.bat
------------------
    Script creates executable and portable files ready for installation.
    The build includes updating html help and readme files.
    Output files are located in "install/vA.B.C.D" catalog,
    where A.B.C.D is current project version.


optimize-svg-in-resources.bat
-----------------------------
    Additional script that optimizes SVG files using "scour".
    For proper work install https://github.com/scour-project/scour
    before executing script.


====


build-portable.bat
------------------
    Script builds portable zip archives for current project version.
    Output files are located in "install/vA.B.C.D" catalog,
    where A.B.C.D is current project version.
    Called from _build-release.bat


build-installers.bat
--------------------
    Script builds installation executables for current project version.
    Output files are located in "install/vA.B.C.D" catalog,
    where A.B.C.D is current project version.
    Called from _build-release.bat


build-help-html.bat
-------------------
    Script updates html files for help in markdown located in "help" catalog.
    Called from build-portable.bat


build-readme-html.bat
---------------------
    Script updates html files for readme in markdown located in root catalog.
    Called from build-portable.bat


build-hashes.bat
----------------
    Script calculates hashes for existing executables of current
    project version and writes it to hashes.txt next to executables.
    Called from _build-release.bat