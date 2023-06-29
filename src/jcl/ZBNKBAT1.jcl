//MFIDEMO  JOB MFIDEMO,MFIDEMO,CLASS=A,MSGCLASS=A,NOTIFY=MFIDEMO        00000100
//* RESTART=SORT                                                        00000200
//* ******************************************************************  00000300
//* ZBNKBAT1.JCL                                                        00000400
//* ******************************************************************  00000500
//UNLOAD   PROC HIQUAL1='MFI01',HIQUAL2='MFIDEMO',REQUEST=''            00000600
//UNLOAD   EXEC PGM=ZBNKEXT1,PARM=&REQUEST                              00000700
//*TEPLIB  DD DSN=&HIQUAL1..&HIQUAL2..LOADLIB,DISP=SHR                  00000800
//SYSOUT   DD SYSOUT=*                                                  00000900
//EXTRACT  DD DSN=&HIQUAL1..&HIQUAL2..BANKEXT1(+1),                     00001000
//            DISP=(NEW,CATLG,DELETE),                                  00001100
//            DCB=(RECFM=VB,LRECL=99,BLKSIZE=990),                      00001200
//            UNIT=SYSDA,SPACE=(TRK,(2,1),RLSE)                         00001300
//BNKACC   DD DSN=&HIQUAL1.V.&HIQUAL2..BNKACC,DISP=SHR                  00001400
//BNKATYP  DD DSN=&HIQUAL1.V.&HIQUAL2..BNKATYPE,DISP=SHR                00001500
//BNKCUST  DD DSN=&HIQUAL1.V.&HIQUAL2..BNKCUST,DISP=SHR                 00001600
//BNKTXN   DD DSN=&HIQUAL1.V.&HIQUAL2..BNKTXN,DISP=SHR                  00001700
//         PEND                                                         00001800
//EXTRACT  EXEC UNLOAD,REQUEST=                                         00001900
//* ******************************************************************  00002000
//SORT     EXEC PGM=SORT                                                00002100
//EXITLIB  DD DSN=MFI01.MFIDEMO.LOADLIB,DISP=SHR                        00002200
//SYSOUT   DD SYSOUT=*                                                  00002300
//SORTIN   DD DSN=MFI01.MFIDEMO.BANKEXT1(+1),DISP=SHR                   00002400
//*ORTIN   DD DSN=MFI01.MFIDEMO.BANKEXT1(0),DISP=SHR                    00002500
//SORTOUT  DD DSN=MFI01.MFIDEMO.BANKSRT1(+1),DISP=(NEW,CATLG,DELETE),   00002600
//            DCB=(RECFM=VB,LRECL=99,BLKSIZE=990),                      00002700
//            UNIT=SYSDA,SPACE=(TRK,(2,1),RLSE)                         00002800
//SYSIN    DD DSN=MFI01.MFIDEMO.CTLCARDS(KBNKSRT1),DISP=SHR             00002900
//* ******************************************************************  00003000
//*PRINT    EXEC YBNKPRT1,GEN='+1',PRM='HELLO WORLD'                    00003100
//*PRINT    EXEC YBNKPRT1,GEN='+1',PRM='EMAIL'                          00003200
//*                                                                     00003300
//* *** $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     00003400
