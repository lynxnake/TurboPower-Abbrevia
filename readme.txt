TurboPower Abbrevia

Table of contents

1.  Introduction
2.  Package names
3.  Installation
4.  Support 
5.  Bug Reporting
5.1  Known Bugs
6.  Version history
6.1   Release 3.05 -Bug Fixes and Enhancements Beta #4
6.2   Release 3.05 -Bug Fixes and Enhancements Beta #3
6.3   Release 3.05 -Bug Fixes and Enhancements Beta #2
6.4   Release 3.05 -Bug Fixes and Enhancements Beta #1


==============================================


1. Introduction


Abbrevia is a compression toolkit for Borland Delphi, C++Builder, &
Kylix. It supports PKZIP 4, Microsoft CAB, TAR, & gzip formats & the
creation of self-extracting archives. It includes visual components
that simplify the manipulation of ZIP files.


This is a source-only release of TurboPower Abbrevia. It includes
designtime and runtime packages for Delphi 3 through 7 and C++Builder
3 through 6 and Kylix 3.

==============================================

2. Package names


TurboPower Abbrevia package names have the following form:

  BNNNKKVV.*
   |  | |
   |  | +------ VV  VCL version (30=Delphi 3, 40=Delphi 4, 70=Delphi 7, K3=Kylix 3)
   |  +-------- KK  Kind of package (R=runtime, D=designtime, CD = CLX designtime,
   |                                 CR = CLX runtime, VD = VCL designtime,
   |                                 VR = VCL runtime)
   |
   +----------- NNN Product version number (e.g., 305=version 3.05)


For example, the Abbrevia designtime package files for Delphi 5 have
the filename B305_D50.*.

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
4.  Support 

     Support for Abbrevia can be obtained through the 
"turbocontrol.abbrevia" newsgroup at news://news.turbocontrol.com.

Outlook express can be setup to read these newsgroups if you don't
already have a newsgroup reader.

==============================================

5.  Bug Reporting

    All bugs and feature requests are handled via the SF.NET trackers.
    If you have a found a bug please post it here so it does not get lost.
    http://sourceforge.net/tracker/?group_id=71001

    Please post complete steps, with code that duplicates the problem.  

==============================================
5.1  Known Bugs

Below is a list of all reported bugs as of this release, not all bugs have been verified, or
duplicated.  If you run into a bug listed below please add a comment with code needed 
to duplicate, and and attach any associated archive files.     

If you find a fix to any of these bugs please attached the fixed code, so it can 
be integrated in to future builds.

 874025 AV during InflateDynamicBlock   
 861543 OnArchiveProgress in TAbSpanStream.Write and User Abort 
 822243 GZip decompress problem 
 785269 Access violation on free 
 785249 ProcessItemFailure not called 
 783613 Corrupt archives when disk spanning 
 763286 No Addition of USES Files 
 752491 Impossible to unzip if basedirectory contains european chars 
 713126 No build Delphi 6 personal 
 698184 "Canvas does not allow drawing" error in TAbMeter 
 698180 bad Gzip causes infinite loop in Deflate engine 
 698176 Problems with CAB and LZX compression 
 698175 Improvements to ZIP identification and interpretation 
 698179 Abbrevia 3 COM object not exposing "WithEvents" interface 
 698178 Off-by-one error(?) in Deflate engine 
 698174 Abbrevia handling of read-only media inadequate 
 698169 Abbrevia not handling Mac created .ZIPs correctly 
 698172 Abbrevia doesn't archive empty folders (other ZIP utilities do) 
 698167 No provision for setting compression level 
 698168 Abbrevia not handling self extracting ZIPs created by SFX 
 698165 Issues with Compound File Class 
 698162 Failed unzipping of password protected files clobbers original 

==============================================

6. Version history

==============================================

6.1   Release 3.05 -Bug Fixes and Enhancements Beta #4
 
 [885672] Current code in CVS fails to build   
 [887793] Current CVS code does not build on D5   
 [887909] soFreshen isn't working   
 [858209] GZip from stream to stream with TAbGzipArchive renders error 
 [858945] copy of File saved to floppy cannot be opened 
 [719083] Windows exe signature check 
 [887889] GZip ExtractAt() extract to wrong locatation   
 [871613] AB305B - QT dependancy in AbGzTyp   

==============================================

6.2   Release 3.05 -Bug Fixes and Enhancements Beta #3
 
 [884897] Invalid CRC32 and Size Values for GZip Archive   
 [880505] setting Windows file attributes on an open file   
 [808499] Wrong file dates in zip archives under Linux 
 [753982] Spanned and split files can be created and read correctly. 
 [698170] Abbrevia's spanning logic is not in conformance with PkWare spec.
 [783617] Spanned and split files created and read correctly. 
 [783614] Error reporting not right 

==============================================

6.3   Release 3.05 -Bug Fixes and Enhancements Beta #2

 NOTE: Disk Spanning and Splitting does not work at all in this release.
 
 Bug Fixes
 ----------
 [785769] V3.05 Beta 1. Problem when unzipping.
 [806077] Unable to Call TestTagged on new Archives
 [806087] DiskFileName needs to be set from FileName. (Bug and Fix from Jeff Rafter)
 [800130] ZIP - Potential Memory Leak
 [799438] A/V if ZIP Filename only contains extension.
 [783583] In TAbZipArchive.FixName() lValue is not set from Value.
 [735109] Spanning not working, access violation
 [714944] AV on Disk Span, ignores OnRequestBlankDisk
 [699140] OnArchive[Save]Progress events don't fire for new archives
 [699119] OnProcessItemFailure does not fire for write locked files

Enhancements
----------------------
  None.

==============================================

6.4   Release 3.05 -Bug Fixes and Enhancements Beta #1

    Bug Fixes
    ---------

       [ 698173 ] Abbrevia doesn't handle filenames containing non-ASCII characters cleanly
       [ 698181 ] Compiler Define out of Place
       [ 698182 ] AV if delete AbMeter before deleting AbVCLMeterLink
       [ 698183 ] CAB Extract At with new name does not work
       [ 698185 ] Wrong file type icons shown
       [ 698186 ] AV if invalid filename specified
       [ 698996 ] Fails to compile in Kylix 3 Pro      
       [ 699595 ] AbZipper and AbZipKit EReadError
       [ 776882 ] Unzip truncates file on some machines
       [ 783203 ] GZIP decompressor not Compliant
 
    Enhancements
    ------------

        [ 702449 ] Create DUnit Framework to test Abbrevia
        [ 783192 ] Add Support Kylix 3
        
NOTE: The number in brackets is the Bug and Enchancement Tracker IDs from SF.NET 