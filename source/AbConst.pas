(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* Abbrevia: Abconst.pas 3.05                            *}
{*********************************************************}
{* Abbrevia: Constants                                   *}
{*********************************************************}

unit AbConst;

interface

const
  AbVersion = '3.05';
  Ab_MessageLen = 255;
  Ab_CaptionLen = 80;
  AB_ZIPPATHDELIM = '/';

const
  AbZipVersionNeeded             = 1;
  AbUnknownCompressionMethod     = 2;
  AbNoExtractionMethod           = 3;
  AbInvalidPassword              = 4;
  AbNoInsertionMethod            = 5;
  AbInvalidFactor                = 6;
  AbDuplicateName                = 7;
  AbUnsupportedCompressionMethod = 8;
  AbUserAbort                    = 9;
  AbArchiveBusy                  = 10;
  AbDiskRequest                  = 11;
  AbLastDiskRequest              = 12;
  AbBadSpanStream                = 13;
  AbDiskNumRequest               = 14;
  AbNoOverwriteSpanStream        = 15;
  AbNoSpannedSelfExtract         = 16;
  AbBlankDisk                    = 17;
  AbStreamFull                   = 18;
  AbNoSuchDirectory              = 19;
  AbInflateBlockError            = 20;
  AbBadStreamType                = 21;
  AbTruncateError                = 22;
  AbZipBadCRC                    = 23;
  AbZipBadStub                   = 24;
  AbFileNotFound                 = 25;
  AbInvalidLFH                   = 26;
  AbNoArchive                    = 27;
  AbErrZipInvalid                = 28;
  AbReadError                    = 29;
  AbInvalidIndex                 = 30;
  AbInvalidThreshold             = 31;
  AbNthImageRequest              = 32;
  AbLastImageRequest             = 33;
  AbImageRequest                 = 34;

  AbUnhandledFileType            = 35;
  AbSpanningNotSupported         = 36;
  AbImageNumRequest              = 37;
  AbLogCreateError               = 38;

  AbBBSReadTooManyBytes          = 40;
  AbBBSSeekOutsideBuffer         = 41;
  AbBBSInvalidOrigin             = 42;
  AbBBSWriteTooManyBytes         = 43;

  AbNoCabinetDllError            = 50;
  AbFCIFileOpenError             = 51;
  AbFCIFileReadError             = 52;
  AbFCIFileWriteError            = 53;
  AbFCIFileCloseError            = 54;
  AbFCIFileSeekError             = 55;
  AbFCIFileDeleteError           = 56;
  AbFCIAddFileError              = 57;
  AbFCICreateError               = 58;
  AbFCIFlushCabinetError         = 59;
  AbFCIFlushFolderError          = 60;
  AbFDICopyError                 = 61;
  AbFDICreateError               = 62;
  AbInvalidCabTemplate           = 63;
  AbInvalidCabFile               = 64;

  AbSWSNotEndofStream            = 80;
  AbSWSSeekFailed                = 81;
  AbSWSWriteFailed               = 82;
  AbSWSInvalidOrigin             = 83;
  AbSWSInvalidNewOrigin          = 84;

  AbVersionFormat                = 100;
  AbMethod                       = 101; {base for later ids, don't add to array!}
  AbMethod0                      = 101;
  AbMethod1                      = 102;
  AbMethod2                      = 103;
  AbMethod3                      = 104;
  AbMethod4                      = 105;
  AbMethod5                      = 106;
  AbMethod6                      = 107;
  AbMethod7                      = 108;
  AbMethod8                      = 109;
  AbMethod9                      = 110;
  AbMethod10                      = 111;
  AbMethod11                      = 112;

  AbCompressedSizeFormat          = 113;
  AbUncompressedSizeFormat        = 114;
  AbCompressionMethodFormat       = 115;
  AbCompressionRatioFormat        = 116;
  AbCRCFormat                     = 117;
  AbReadOnly                      = 118;
  AbHidden                        = 119;
  AbSystem                        = 120;
  AbArchived                      = 121;
  AbEFAFormat                     = 122;
  AbIFAFormat                     = 123;
  AbText                          = 124;
  AbBinary                        = 125;
  AbEncryptionFormat              = 126;
  AbEncrypted                     = 127;
  AbNotEncrypted                  = 128;
  AbUnknown                       = 129;
  AbTimeStampFormat               = 130;
  AbMadeByFormat                  = 131;
  AbNeededFormat                  = 132;
  AbCommentFormat                 = 133;

  AbDefaultExt                    = 134;
  AbFilter                        = 135;
  AbFileNameTitle                 = 136;

  AbOK                            = 137;
  AbCancel                        = 138;
  AbSelectDirectory               = 139;

  AbEnterPassword                 = 140;
  AbPassword                      = 141;
  AbVerify                        = 142;

  AbCabExt                        = 150;
  AbCabFilter                     = 151;
  AbLogExt                        = 152;
  AbLogFilter                     = 153;
  AbExeExt                        = 154;
  AbExeFilter                     = 155;

  AbVMSReadTooManyBytes           = 200;
  AbVMSInvalidOrigin              = 201;
  AbVMSErrorOpenSwap              = 202;
  AbVMSSeekFail                   = 203;
  AbVMSReadFail                   = 204;
  AbVMSWriteFail                  = 205;
  AbVMSWriteTooManyBytes          = 206;

  AbDefColHeadings                = 250;
  AbItemNameHeading               = 250;
  AbPackedHeading                 = 251;
  AbMethodHeading                 = 252;
  AbRatioHeading                  = 253;
  AbCRCHeading                    = 254;
  AbFileAttrHeading               = 255;
  AbFileFormatHeading             = 256;
  AbEncryptionHeading             = 257;
  AbTimeStampHeading              = 258;
  AbFileSizeHeading               = 259;
  AbVersionMadeHeading            = 260;
  AbVersionNeededHeading          = 261;
  AbPathHeading                   = 262;
  AbPartialHeading                = 263;
  AbExecutableHeading             = 264;

  AbCabMethod0                    = 290;
  AbCabMethod1                    = 291;

  AbLtAdd                         = 310;
  AbLtDelete                      = 311;
  AbLtExtract                     = 312;
  AbLtFreshen                     = 313;
  AbLtMove                        = 314;
  AbLtReplace                     = 315;
  AbLtStart                       = 316;

  AbGZipInvalid                   = 400;
  AbGzipBadCRC                    = 401;
  AbGzipBadFileSize               = 402;

  AbUnhandledEntity               = 513;

implementation

end.
