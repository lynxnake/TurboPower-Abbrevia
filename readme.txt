TurboPower Abbrevia


Table of contents

1.  Introduction
2.  Package names
3.  Installation
4.  Version history
4.1   Release 3.04

==============================================


1. Introduction


Abbrevia is a compression toolkit for Borland Delphi, C++Builder, &
Kylix. It supports PKZIP 4, Microsoft CAB, TAR, & gzip formats & the
creation of self-extracting archives. It includes visual components
that simplify the manipulation of ZIP files.


This is a source-only release of TurboPower Abbrevia. It includes
designtime and runtime packages for Delphi 3 through 7 and C++Builder
3 through 6.

==============================================

2. Package names


TurboPower Abbrevia package names have the following form:

  BNNNKKVV.*
   |  | |
   |  | +------ VV  VCL version (30=Delphi 3, 40=Delphi 4, 70=Delphi 7)
   |  +-------- KK  Kind of package (R=runtime, D=designtime, CD = CLX designtime,
   |                                 CR = CLX runtime, VD = VCL designtime,
   |                                 VR = VCL runtime)
   |
   +----------- NNN Product version number (e.g., 304=version 3.04)


For example, the Abbrevia designtime package files for Delphi 5 have
the filename B304_D50.*.

==============================================

3. Installation


To install TurboPower Abbrevia into your IDE, take the following steps:

  1. Unzip the release files into a directory (e.g., d:\abbrevia).

  2. Start Delphi or C++Builder.

  3. Add the source subdirectory (e.g., d:\abbrevia\source) to the
     IDE's library path.

  4. Open & compile the runtime package specific to the IDE being
     used.

  5. Open & install the designtime package specific to the IDE being
     used. The IDE should notify you the components have been
     installed.

  6. Make sure the PATH environmental variable contains the directory
     in which the compiled packages (i.e., BPL or DPL files) were
     placed.

==============================================

4. Version history


4.1 Release 3.04

    Bug Fixes
    ---------

    - Problem with redundant checks for Central Directory Tail on spanned archives.

    - Removed AbZipTyp dependency on Dialogs unit.


    Enhancements
    ------------

    - Delphi 7 Support Added

    - Added Support for Archive Save Progress
