TurboPower Abbrevia

Table of contents

1.  Introduction
2.  Current Plans and Priorities
3.  Package names
4.  Installation
5.  Support 
6.  Bug Reporting
7.  Version history
7.1  3.05 Beta #1
7.2  3.05 Beta #2
7.3  3.05 Beta #3
7.4  3.05 Beta #4
7.5  3.05 Beta #5
7.6  3.05 Beta #6 (Everyone should read this section!)
7.7  3.05 Release Candidate


==============================================

1. Introduction

Abbrevia is a compression toolkit for Embarcadero Delphi, C++Builder,
and Kylix. It supports PKZIP 4, Microsoft CAB, TAR, and gzip formats
and the creation of self-extracting archives. It includes visual
components that simplify the manipulation of ZIP files.

This is a source-only release of TurboPower Abbrevia. It includes
design-time and run-time packages for Delphi 6 through 2010 and Kylix 3.
C++Builder support is untested in the current version.

==============================================

2. Current Plans and Priorities

This release adds Delphi 2009/2010 support, along with various other minor
fixes and enhancements.  In Delphi 2009/2010 the filenames are all Unicode
strings, but the archives themselves still only support ANSI paths.

I'm holding off on significant new features and large code changes until after
this release is finalized.  If no new issues are found this release candidate
will become 3.05.  I won't consider a bug a blocker unless it was
introduced after 3.05 beta 6.  If anyone disagrees they're welcome to submit
patches.

After 3.05 is released work can begin on major features.  I intend to work on
them in roughly this order:
  * Unicode filenames
  * Spanned/split zip improvements
  * Bzip2 archives using C .obj files
  * GNU TAR long filename extensions
  * FreePascal/Delphi 2011 compatibility
  * 64-bit zip support
  * Other archive formats (7-zip/RAR/etc) either in Delphi or using C DLLs

I'm also planning on reworking things to remove the zip extraction conditional
defines introduced in 3.05 beta 6.

If you have questions/comments/suggestions, refer to the "Support" and "Bug
Reporting" sections below.  I welcome any feedback you have.

==============================================

3. Package names

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

4. Installation

To install TurboPower Abbrevia into your IDE, take the following steps:

  1. Unzip the release files into a directory (e.g., d:\abbrevia).

  2. Start Delphi or C++Builder.

  3. Add the source subdirectory (e.g., d:\abbrevia\source) to the
     IDE's library path.

  4. Open & compile the runtime package specific to the IDE being
     used (e.g. B305vr2007.dpk for Delphi2007)

  5. Open & install the designtime package specific to the IDE being
     used (e.g. B305vd2007.dpk for Delphi2007). The IDE should notify 
     you the components have been installed.

  6. Make sure the PATH environmental variable contains the directory
     in which the compiled packages (i.e., BPL or DPL files) were
     placed.

==============================================

5.  Support 

Support for Abbrevia can be obtained through the "Help" forum on 
the SourceForge site at
  https://sourceforge.net/projects/tpabbrevia/forums/forum/241865
or in the "turbocontrol.abbrevia" newsgroup at
  news://news.turbocontrol.com.

Outlook express can be setup to read these newsgroups if you don't
already have a newsgroup reader.

Access the support newsgroups with a browser:
   http://delphi.newswhat.com/geoxml/forumlistthreads?groupname=turbocontrol.abbrevia

==============================================

6.  Bug Reporting

All bugs and feature requests are handled via the SF.NET trackers.  If
you have a found a bug please post it here so it does not get lost.
  http://sourceforge.net/tracker/?group_id=71001

Please post complete steps, with code that duplicates the problem.  If
the problem only affects specific files please attach them to the bug
report, or, if they need to be kept confidential, email them to one of
the project administrators.

Please note we can not read your mind, unreported bugs will not be
fixed.

==============================================

7. Version history

Below you will find the history of the 3.05 Beta Releases.
The number in brackets is the Bug and Enchancement Tracker IDs from SF.NET. 


================

7.1   3.05 Beta #1

  Bug Fixes
  ---------
  [698173] Abbrevia doesn't handle filenames containing non-ASCII characters cleanly
  [698181] Compiler Define out of Place
  [698182] AV if delete AbMeter before deleting AbVCLMeterLink
  [698183] CAB Extract At with new name does not work
  [698185] Wrong file type icons shown
  [698186] AV if invalid filename specified
  [698996] Fails to compile in Kylix 3 Pro      
  [699595] AbZipper and AbZipKit EReadError
  [776882] Unzip truncates file on some machines
  [783203] GZIP decompressor not Compliant
   
  Enhancements
  ------------
  [702449] Create DUnit Framework to test Abbrevia
  [783192] Add Support Kylix 3
          

================

7.2   3.05 Beta #2

NOTE: Disk Spanning and Splitting does not work at all in this release.
  
  Bug Fixes
  ----------
  [785769] V3.05 Beta 1. Problem when unzipping.
  [806077] Unable to Call TestTagged on new Archives
  [806087] DiskFileName needs to be set from FileName.
  [800130] ZIP - Potential Memory Leak
  [799438] A/V if ZIP Filename only contains extension.
  [783583] In TAbZipArchive.FixName() lValue is not set from Value.
  [735109] Spanning not working, access violation
  [714944] AV on Disk Span, ignores OnRequestBlankDisk
  [699140] OnArchive[Save]Progress events don't fire for new archives
  [699119] OnProcessItemFailure does not fire for write locked files
 
  Enhancements
  ------------
  None.

 
================

7.3   3.05 Beta #3
  
  [884897] Invalid CRC32 and Size Values for GZip Archive   
  [880505] setting Windows file attributes on an open file   
  [808499] Wrong file dates in zip archives under Linux 
  [753982] Spanned and split files can be created and read correctly. 
  [698170] Abbrevia's spanning logic is not in conformance with PkWare spec.
  [783617] Spanned and split files created and read correctly. 
  [783614] Error reporting not right 
 

================

7.4   3.05 Beta #4
 
  [885672] Current code in CVS fails to build   
  [887793] Current CVS code does not build on D5   
  [887909] soFreshen isn't working   
  [858209] GZip from stream to stream with TAbGzipArchive renders error 
  [858945] copy of File saved to floppy cannot be opened 
  [719083] Windows exe signature check 
  [887889] GZip ExtractAt() extract to wrong locatation   
  [871613] AB305B - QT dependancy in AbGzTyp   


================

7.5   3.05 Beta #5

  [785269] Access violation on free 
  [885670] Memory corruption on damaged Zip file   
  [890888] Memory Leak in abZipPrc   
  [891007] Compiler warning in AbZipTyp   
  [889324] problem closing TAbUnzip archive   
  [888927] compiler warnings with current CVS code   


================

7.6   3.05 Beta #6

  [906875] IDE hangs when closing form with TAbVCLMeterLink  
  [892830] freshing file it doesn't set the correct Item.DiskFileName 
  [752491] Impossible to unzip if basedirectory contains european chars 
  [698162] Failed unzipping of password protected files clobbers original 
  [912918] Files Extracted to Network Drives may be truncated 

In order to fix bugs [698162] and [912918] a new set of defines were
added to AbDefine.inc that allow you to control the way Abbrevia
extracts files.  This is a temporary hack and I will be refactoring the
code to make them redundant after 3.05 is released.  The defines only
affect AbUnZip in AbUnzPrc.pas.

When extracting files, we have three options:

  1. Clobber - Write in place.  This is the original behavior, and the
       side effect is that bug [698162] will reappear.  If you try to
       overwrite a file by extracting from a password protected zip and
       aren't able to give the correct password the original file is
       deleted.

  2. Memory - File is extracted to memory, and if successful is then
       written to disk.

  3. TempFile - File is extract to the temp directory, and if
       successful is then moved to correct location.

In testing, the memory option is the fastest as long as you don't end
up using the page file.   Once you start to page, this option is
usually (not always) slower than TempFile method.  Clobber was not
tested for performance, it was the original method and buggy.  

The TempFile method is the current default behavior.


================

7.7   3.05 Release Candidate

  Bug Fixes
  ---------
  [ 2896382 ] CAB saving now occurs in Save instead of the destructor.
  [ 2900175 ] Fixed double free in TAbCabView.
  Fixed file handle leak when trying to open an invalid CAB file.
  Removed unused TAbZipArchive.SaveArchive2 procedure.
  Fixed unReduce support (Pkzip 0.9x).
  Fixed renaming in zips to set the VersionMadeBy field.
  Fixed AbDosFileDateToDateTime returning the wrong seconds value.
  Removed unused trial code.
  [ 1327858 ] Last modified times on Unix are off by 24 hours.
  Fixed Linux version storing temp files in /etc instead of /tmp.
  Fixed file handle leak when opening temp file in Linux.
  [ 1163035 ] Fixed invalid stream errors on Linux.
  [ 1939093 ] Fixed CAB creationg storing files without path information.
  Fixed CAB extraction so it restores DOS attributes and last modified times.
  [ 887889, 976804 ] GZip ignores BaseDirectory or target path when extracting.
  [ 1690507 ] Fixed resource leak in AbUnzPrc.DoExtractStored

  Enhancements
  ------------
  Added support for Delphi 2007, 2009, and 2010.
  Added ExtractToStream support for CAB archives.
  Added support for creating and modifying gzipped tars.
  [ 698172 ] Added support for storing and extracting empty directories in zips.
  Zip and Gzip ExtraField properties are now classes instead of strings.
  Changed behavior to match GNU gzip in places where it differs from RFC 1952.
  Added additional values for gzip header's OS field.
  Added support for sorting grid by path.
  Added support for a custom signature for compound files.
  Compound files now supports path strings wihtout '/'.
  Removed support for Windows 95 and Delphi 1-5.
