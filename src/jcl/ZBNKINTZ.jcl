//MFIDEMO  JOB MFIDEMO,MFIDEMO,CLASS=A,MSGCLASS=A,NOTIFY=MFIDEMO        00000100
//* ******************************************************************  00000200
//* ZBNKINTZ.JCL                                                        00000300
//* ******************************************************************  00000400
//* STEP01 - DEFINE VSAM FILES REQUIRED IF USING CICS - BNKACC          00000500
//* ******************************************************************  00000600
//STEP01   EXEC PGM=IDCAMS                                              00000700
//SYSPRINT DD SYSOUT=*                                                  00000800
//SYSIN    DD *                                                         00000900
  DELETE MFI01V.MFIDEMO.HOST.BNKACC CLUSTER PURGE                       00001000
  SET    MAXCC=0                                                        00001100
  DEFINE CLUSTER (NAME(MFI01V.MFIDEMO.HOST.BNKACC) -                    00001200
                  SHAREOPTIONS(2 3)-                                    00001300
                  STORCLAS(TEMP) -                                      00001400
                  INDEXED -                                             00001500
                  RECORDS(100 20)) -                                    00001600
            DATA (NAME(MFI01V.MFIDEMO.HOST.BNKACC.DATA) -               00001700
                  KEYS(9 5) -                                           00001800
                  RECORDSIZE(100 100)) -                                00001900
            INDEX (NAME(MFI01V.MFIDEMO.HOST.BNKACC.INDEX) -             00002000
                   IMBED)                                               00002100
  REPRO INDATASET(MFI01.MFIDEMO.UNLOADED.SQL.BNKACC) -                  00002200
        OUTDATASET(MFI01V.MFIDEMO.HOST.BNKACC)                          00002300
  DEFINE AIX (NAME(MFI01V.MFIDEMO.HOST.BNKACC.AIX1)  -                  00002400
              RELATE(MFI01V.MFIDEMO.HOST.BNKACC)     -                  00002500
              SHAREOPTIONS(2 3) -                                       00002600
              RECORDS(100 20) -                                         00002700
              KEYS(5 0) -                                               00002800
              RECORDSIZE(100 100) -                                     00002900
              NONUNIQUEKEY -                                            00003000
              UPGRADE)                                                  00003100
  DEFINE PATH (NAME(MFI01V.MFIDEMO.HOST.BNKACC.PATH1) -                 00003200
               PATHENTRY(MFI01V.MFIDEMO.HOST.BNKACC.AIX1))              00003300
  BLDINDEX INDATASET(MFI01V.MFIDEMO.HOST.BNKACC) -                      00003400
           OUTDATASET(MFI01V.MFIDEMO.HOST.BNKACC.AIX1)                  00003500
//* ******************************************************************  00001000
//* STEP02 - DEFINE VSAM FILES REQUIRED IF USING CICS - BNKATYPE        00001100
//* ******************************************************************  00001200
//STEP02   EXEC PGM=IDCAMS                                              00001300
//SYSPRINT DD SYSOUT=*                                                  00001400
//SYSIN    DD *                                                         00001500
  DELETE MFI01V.MFIDEMO.HOST.BNKATYPE CLUSTER PURGE                     00004200
  SET    MAXCC=0                                                        00004300
  DEFINE CLUSTER (NAME(MFI01V.MFIDEMO.HOST.BNKATYPE) -                  00004400
                  SHAREOPTIONS(2 3)-                                    00004500
                  STORCLAS(TEMP) -                                      00004600
                  INDEXED -                                             00004700
                  RECORDS(100 20)) -                                    00004800
            DATA (NAME(MFI01V.MFIDEMO.HOST.BNKATYPE.DATA) -             00004900
                  KEYS(1 0) -                                           00005000
                  RECORDSIZE(100 100)) -                                00005100
            INDEX (NAME(MFI01V.MFIDEMO.HOST.BNKATYPE.INDEX) -           00005200
                   IMBED)                                               00005300
  REPRO INDATASET(MFI01.MFIDEMO.UNLOADED.SQL.BNKATYPE) -                00005400
        OUTDATASET(MFI01V.MFIDEMO.HOST.BNKATYPE)                        00005500
//* ******************************************************************  00001600
//* STEP03 - DEFINE VSAM FILES REQUIRED IF USING CICS - BNKCUST         00001700
//* ******************************************************************  00001800
//STEP01   EXEC PGM=IDCAMS                                              00001900
//SYSPRINT DD SYSOUT=*                                                  00002000
//SYSIN    DD *                                                         00002100
  DELETE MFI01V.MFIDEMO.HOST.BNKCUST CLUSTER PURGE                      00006200
  SET    MAXCC=0                                                        00006300
  DEFINE CLUSTER (NAME(MFI01V.MFIDEMO.HOST.BNKCUST) -                   00006400
                  SHAREOPTIONS(2 3)-                                    00006500
                  STORCLAS(TEMP) -                                      00006600
                  INDEXED -                                             00006700
                  RECORDS(100 20)) -                                    00006800
            DATA (NAME(MFI01V.MFIDEMO.HOST.BNKCUST.DATA) -              00006900
                  KEYS(5 0) -                                           00007000
                  RECORDSIZE(250 250)) -                                00007100
            INDEX (NAME(MFI01V.MFIDEMO.HOST.BNKCUST.INDEX) -            00007200
                   IMBED)                                               00007300
  REPRO INDATASET(MFI01.MFIDEMO.UNLOADED.SQL.BNKCUST) -                 00007400
        OUTDATASET(MFI01V.MFIDEMO.HOST.BNKCUST)                         00007500
  DEFINE AIX (NAME(MFI01V.MFIDEMO.HOST.BNKCUST.AIX1)  -                 00007600
              RELATE(MFI01V.MFIDEMO.HOST.BNKCUST)     -                 00007700
              SHAREOPTIONS(2 3) -                                       00007800
              RECORDS(100 20) -                                         00007900
              KEYS(25 5) -                                              00008000
              RECORDSIZE(250 250) -                                     00008100
              NONUNIQUEKEY -                                            00008200
              UPGRADE)                                                  00008300
  DEFINE PATH (NAME(MFI01V.MFIDEMO.HOST.BNKCUST.PATH1) -                00008400
               PATHENTRY(MFI01V.MFIDEMO.HOST.BNKCUST.AIX1))             00008500
  BLDINDEX INDATASET(MFI01V.MFIDEMO.HOST.BNKCUST) -                     00008600
           OUTDATASET(MFI01V.MFIDEMO.HOST.BNKCUST.AIX1)                 00008700
  DEFINE AIX (NAME(MFI01V.MFIDEMO.HOST.BNKCUST.AIX2)  -                 00008800
              RELATE(MFI01V.MFIDEMO.HOST.BNKCUST)     -                 00008900
              SHAREOPTIONS(2 3) -                                       00009000
              RECORDS(100 20) -                                         00009100
              KEYS(25 30) -                                             00009200
              RECORDSIZE(250 250) -                                     00009300
              NONUNIQUEKEY -                                            00009400
              UPGRADE)                                                  00009500
  DEFINE PATH (NAME(MFI01V.MFIDEMO.HOST.BNKCUST.PATH2) -                00009600
               PATHENTRY(MFI01V.MFIDEMO.HOST.BNKCUST.AIX2))             00009700
  BLDINDEX INDATASET(MFI01V.MFIDEMO.HOST.BNKCUST) -                     00009800
           OUTDATASET(MFI01V.MFIDEMO.HOST.BNKCUST.AIX2)                 00009900
//* ******************************************************************  00002200
//* STEP04 - DEFINE VSAM FILES REQUIRED IF USING CICS - BNKTXN          00002300
//* ******************************************************************  00002400
//STEP04   EXEC PGM=IDCAMS                                              00002500
//SYSPRINT DD SYSOUT=*                                                  00002600
//SYSIN    DD *                                                         00002700
  DELETE MFI01V.MFIDEMO.HOST.BNKTXN CLUSTER PURGE                       00010600
  SET    MAXCC=0                                                        00010700
  DEFINE CLUSTER (NAME(MFI01V.MFIDEMO.HOST.BNKTXN) -                    00010800
                  SHAREOPTIONS(2 3)-                                    00010900
                  STORCLAS(TEMP) -                                      00011000
                  INDEXED -                                             00011100
                  RECORDS(100 20)) -                                    00011200
            DATA (NAME(MFI01V.MFIDEMO.HOST.BNKTXN.DATA) -               00011300
                  KEYS(26 16) -                                         00011400
                  RECORDSIZE(400 400)) -                                00011500
            INDEX (NAME(MFI01V.MFIDEMO.HOST.BNKTXN.INDEX) -             00011600
                   IMBED)                                               00011700
  REPRO INDATASET(MFI01.MFIDEMO.UNLOADED.SQL.BNKTXN) -                  00011800
        OUTDATASET(MFI01V.MFIDEMO.HOST.BNKTXN)                          00011900
  DEFINE AIX (NAME(MFI01V.MFIDEMO.HOST.BNKTXN.AIX1)  -                  00012000
              RELATE(MFI01V.MFIDEMO.HOST.BNKTXN)     -                  00012100
              SHAREOPTIONS(2 3) -                                       00012200
              RECORDS(100 20) -                                         00012300
              KEYS(35 7) -                                              00012400
              RECORDSIZE(400 400) -                                     00012500
              NONUNIQUEKEY -                                            00012600
              UPGRADE)                                                  00012700
  DEFINE PATH (NAME(MFI01V.MFIDEMO.HOST.BNKTXN.PATH1) -                 00012800
               PATHENTRY(MFI01V.MFIDEMO.HOST.BNKTXN.AIX1))              00012900
  BLDINDEX INDATASET(MFI01V.MFIDEMO.HOST.BNKTXN) -                      00013000
           OUTDATASET(MFI01V.MFIDEMO.HOST.BNKTXN.AIX1)                  00013100
//* ******************************************************************  00002800
//* STEP05 - DEFINE VSAM FILES REQUIRED IF USING CICS - HELP            00002900
//* ******************************************************************  00003000
//STEP01   EXEC PGM=IDCAMS                                              00003100
//SYSPRINT DD SYSOUT=*                                                  00003200
//SYSIN    DD *                                                         00003300
  DELETE MFI01V.MFIDEMO.HOST.BNKHELP CLUSTER PURGE                      00013800
  SET    MAXCC=0                                                        00013900
  DEFINE CLUSTER (NAME(MFI01V.MFIDEMO.HOST.BNKHELP) -                   00014000
                  SHAREOPTIONS(2 3)-                                    00014100
                  STORCLAS(TEMP) -                                      00014200
                  INDEXED -                                             00014300
                  RECORDS(100 20)) -                                    00014400
            DATA (NAME(MFI01V.MFIDEMO.HOST.BNKHELP.DATA) -              00014500
                  KEYS(8 0) -                                           00014600
                  RECORDSIZE(83 83)) -                                  00014700
            INDEX (NAME(MFI01V.MFIDEMO.HOST.BNKHELP.INDEX) -            00014800
                   IMBED)                                               00014900
  REPRO INDATASET(MFI01.MFIDEMO.UNLOADED.SQL.BNKHELP) -                 00015000
        OUTDATASET(MFI01V.MFIDEMO.HOST.BNKHELP)                         00015100
//*                                                                     00003400
//* *** $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     00003500
