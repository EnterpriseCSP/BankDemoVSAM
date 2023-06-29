//MFIDEMO  JOB MFIDEMO,MFIDEMO,CLASS=A,MSGCLASS=X,NOTIFY=MFIDEMO        00000100
//* ******************************************************************  00000200
//* ZBNKINT2.JCL                                                        00000300
//* ******************************************************************  00000400
//* DELETE AND DEFINE VSAM FILES REQUIRED IF USING CICS                 00000500
//* ******************************************************************  00000600
//* STEP01 - DELETE VSAM FILES                                          00000700
//* ******************************************************************  00000800
//STEP01   EXEC PGM=IDCAMS                                              00000900
//SYSPRINT DD SYSOUT=*                                                  00001000
//SYSIN    DD *                                                         00001100
  DELETE MFI01V.MFIDEMO.BNKACC                                          00001200
  DELETE MFI01V.MFIDEMO.BNKATYPE                                        00001300
  DELETE MFI01V.MFIDEMO.BNKCUST                                         00001400
  DELETE MFI01V.MFIDEMO.BNKTXN                                          00001500
  DELETE MFI01V.MFIDEMO.BNKHELP                                         00001600
  SET    MAXCC=0                                                        00001700
//* ******************************************************************  00001200
//* STEP02 - DEFINE VSAM FILES                                          00001300
//* ******************************************************************  00001400
//STEP02   EXEC PGM=IDCAMS                                              00001500
//SYSPRINT DD SYSOUT=*                                                  00001600
//SYSIN    DD *                                                         00001700
  DEFINE CLUSTER (NAME(MFI01V.MFIDEMO.BNKACC)          -                00002400
                  TRACKS(5 1)                          -                00002500
                  VOLUMES(MRNT04)                      -                00002600
                  KEYS(9 5)                            -                00002700
                  RECSZ(100 100)                       -                00002800
                  INDEXED)                             -                00002900
         DATA    (NAME(MFI01V.MFIDEMO.BNKACC.DATA)     -                00003000
                  CISZ(8192))                          -                00003100
         INDEX   (NAME(MFI01V.MFIDEMO.BNKACC.INDEX))                    00003200
  DEFINE CLUSTER (NAME(MFI01V.MFIDEMO.BNKATYPE)        -                00003300
                  TRACKS(5 1)                          -                00003400
                  VOLUMES(MRNT04)                      -                00003500
                  KEYS(1 0)                            -                00003600
                  RECSZ(100 100)                       -                00003700
                  INDEXED)                             -                00003800
         DATA    (NAME(MFI01V.MFIDEMO.BNKATYPE.DATA)   -                00003900
                  CISZ(8192))                          -                00004000
         INDEX   (NAME(MFI01V.MFIDEMO.BNKATYPE.INDEX))                  00004100
  DEFINE CLUSTER (NAME(MFI01V.MFIDEMO.BNKCUST)         -                00004200
                  TRACKS(5 1)                          -                00004300
                  VOLUMES(MRNT04)                      -                00004400
                  KEYS(5 0)                            -                00004500
                  RECSZ(250 250)                       -                00004600
                  INDEXED)                             -                00004700
         DATA    (NAME(MFI01V.MFIDEMO.BNKCUST.DATA)    -                00004800
                  CISZ(8192))                          -                00004900
         INDEX   (NAME(MFI01V.MFIDEMO.BNKCUST.INDEX))                   00005000
                                                                        00005100
  DEFINE CLUSTER (NAME(MFI01V.MFIDEMO.BNKTXN)          -                00005200
                  TRACKS(5 1)                          -                00005300
                  VOLUMES(MRNT04)                      -                00005400
                  KEYS(26 16)                          -                00005500
                  RECSZ(400 400)                       -                00005600
                  INDEXED)                             -                00005700
         DATA    (NAME(MFI01V.MFIDEMO.BNKTXN.DATA)     -                00005800
                  CISZ(8192))                          -                00005900
         INDEX   (NAME(MFI01V.MFIDEMO.BNKTXN.INDEX))                    00006000
  DEFINE CLUSTER (NAME(MFI01V.MFIDEMO.BNKHELP)         -                00006100
                  TRACKS(5 1)                          -                00006200
                  VOLUMES(MRNT04)                      -                00006300
                  KEYS(8 0)                            -                00006400
                  RECSZ(83 83)                         -                00006500
                  INDEXED)                             -                00006600
         DATA    (NAME(MFI01V.MFIDEMO.BNKHELP.DATA)    -                00006700
                  CISZ(8192))                          -                00006800
         INDEX   (NAME(MFI01V.MFIDEMO.BNKHELP.INDEX))                   00006900
  SET MAXCC=0                                                           00007000
//* ******************************************************************  00001800
//* STEP03 - DEFINE ALTERNATE INDEXS AND PATH S                         00001900
//* ******************************************************************  00002000
//STEP03   EXEC PGM=IDCAMS                                              00002100
//SYSPRINT DD SYSOUT=*                                                  00002200
//SYSIN    DD *                                                         00002300
  DEFINE AIX     (NAME(MFI01V.MFIDEMO.BNKACC.AIX1)     -                00007700
                  TRACKS(5 1)                          -                00007800
                  VOLUMES(MRNT04)                      -                00007900
                  RELATE(MFI01V.MFIDEMO.BNKACC)        -                00008000
                  KEYS(5 0))                                            00008100
  DEFINE AIX     (NAME(MFI01V.MFIDEMO.BNKCUST.AIX1)    -                00008200
                  TRACKS(5 1)                          -                00008300
                  VOLUMES(MRNT04)                      -                00008400
                  RELATE(MFI01V.MFIDEMO.BNKCUST)       -                00008500
                  KEYS(25 5))                                           00008600
  DEFINE AIX     (NAME(MFI01V.MFIDEMO.BNKCUST.AIX2)    -                00008700
                  TRACKS(5 1)                          -                00008800
                  VOLUMES(MRNT04)                      -                00008900
                  RELATE(MFI01V.MFIDEMO.BNKCUST)       -                00009000
                  KEYS(25 30))                                          00009100
  DEFINE AIX     (NAME(MFI01V.MFIDEMO.BNKTXN.AIX1)     -                00009200
                  TRACKS(5 1)                          -                00009300
                  VOLUMES(MRNT04)                      -                00009400
                  RELATE(MFI01V.MFIDEMO.BNKTXN)        -                00009500
                  KEYS(35 7))                                           00009600
                                                                        00009700
     DEFINE PATH (NAME(MFI01V.MFIDEMO.BNKACC.PATH1)    -                00009800
                  PENT(MFI01V.MFIDEMO.BNKACC.AIX1))                     00009900
     DEFINE PATH (NAME(MFI01V.MFIDEMO.BNKCUST.PATH1)   -                00010000
                  PENT(MFI01V.MFIDEMO.BNKCUST.AIX1))                    00010100
     DEFINE PATH (NAME(MFI01V.MFIDEMO.BNKCUST.PATH2)   -                00010200
                  PENT(MFI01V.MFIDEMO.BNKCUST.AIX2))                    00010300
     DEFINE PATH (NAME(MFI01V.MFIDEMO.BNKTXN.PATH1)    -                00010400
                  PENT(MFI01V.MFIDEMO.BNKTXN.AIX1))                     00010500
                                                                        00010600
  LISTC  LVL     (MFI01V.MFIDEMO)                                       00010700
  SET MAXCC=0                                                           00010800
//* ******************************************************************  00002400
//* STEP04 - REPRO THE SEQUENTIAL DATA AND DO SELECTED PRINTS           00002500
//* ******************************************************************  00002600
//STEP04   EXEC PGM=IDCAMS                                              00002700
//SYSPRINT DD SYSOUT=*                                                  00002800
//SYSIN    DD *                                                         00002900
  REPRO  IFILE   (BNKACC)   ODS(MFI01V.MFIDEMO.BNKACC)   REPLACE        00011500
  REPRO  IFILE   (BNKATYPE) ODS(MFI01V.MFIDEMO.BNKATYPE) REPLACE        00011600
  REPRO  IFILE   (BNKCUST)  ODS(MFI01V.MFIDEMO.BNKCUST)  REPLACE        00011700
  REPRO  IFILE   (BNKTXN)   ODS(MFI01V.MFIDEMO.BNKTXN)   REPLACE        00011800
  REPRO  IFILE   (BNKHELP)  ODS(MFI01V.MFIDEMO.BNKHELP)  REPLACE        00011900
                                                                        00012000
  PRINT  IDS     (MFI01V.MFIDEMO.BNKACC)    COUNT(3)                    00012100
  PRINT  IDS     (MFI01V.MFIDEMO.BNKATYPE)  COUNT(3)                    00012200
  PRINT  IDS     (MFI01V.MFIDEMO.BNKCUST)   COUNT(3)                    00012300
  PRINT  IDS     (MFI01V.MFIDEMO.BNKTXN)    COUNT(3)                    00012400
  PRINT  IDS     (MFI01V.MFIDEMO.BNKHELP)   COUNT(3)                    00012500
                                                                        00012600
  PRINT  IDS     (ASC.BNKACC)    COUNT(3)                               00012700
  PRINT  IDS     (ASC.BNKATYPE)  COUNT(3)                               00012800
  PRINT  IDS     (ASC.BNKCUST)   COUNT(3)                               00012900
  PRINT  IDS     (ASC.BNKTXN)    COUNT(3)                               00013000
  PRINT  IDS     (ASC.BNKHELP)   COUNT(3)                               00013100
//BNKACC    DD DSN=ASC.BNKACC,DISP=SHR                                  00003000
//BNKATYPE  DD DSN=ASC.BNKATYPE,DISP=SHR                                00003100
//BNKCUST   DD DSN=ASC.BNKCUST,DISP=SHR                                 00003200
//BNKTXN    DD DSN=ASC.BNKTXN,DISP=SHR                                  00003300
//BNKHELP   DD DSN=ASC.BNKHELP,DISP=SHR                                 00003400
//*                                                                     00003500
//* *** $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     00003600
