LZMA SDK v9.20

Original download available from http://downloads.sourceforge.net/sevenzip/lzma920.tar.bz2

Added #pragma pack(push, 1) / #pragma pack(pop) to LzmaDec.h, LzmaEnc.h, Types.h

Compile with
  bcc32 -q -c -u- -w-8004 -w-8065 *.c