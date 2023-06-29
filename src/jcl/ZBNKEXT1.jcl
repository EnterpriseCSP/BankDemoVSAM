//MFIDEMO  JOB MFIDEMO,MFIDEMO,CLASS=A,MSGCLASS=A,NOTIFY=MFIDEMO        00000100
//* ******************************************************************  00000200
//* ZBNKBAT1.JCL                                                        00000300
//* ******************************************************************  00000400
//UNLOAD   PROC HIQUAL1='MFI01',HIQUAL2='MFIDEMO',REQUEST=''            00000500
//UNLOAD   EXEC PGM=ZBNKEXT1,PARM=&REQUEST                              00000600
//STEPLIB  DD DSN=&HIQUAL1..&HIQUAL2..LOADLIB,DISP=SHR                  00000700
//EXTRACT  DD DSN=&HIQUAL1..&HIQUAL2..BANKEXT1(+1),                     00000800
//            DISP=(NEW,CATLG,DELETE),                                  00000900
//            DCB=(RECFM=VB,LRECL=99,BLKSIZE=990),                      00001000
//            UNIT=SYSDA,SPACE=(TRK,(2,1),RLSE)                         00001100
//BNKACC   DD DSN=&HIQUAL1.V.&HIQUAL2..BNKACC,DISP=SHR                  00001200
//BNKATYP  DD DSN=&HIQUAL1.V.&HIQUAL2..BNKATYPE,DISP=SHR                00001300
//BNKCUST  DD DSN=&HIQUAL1.V.&HIQUAL2..BNKCUST,DISP=SHR                 00001400
//BNKTXN   DD DSN=&HIQUAL1.V.&HIQUAL2..BNKTXN,DISP=SHR                  00001500
//         PEND                                                         00001600
//EXTRACT  EXEC UNLOAD                                                  00001700
//*                                                                     00001800
//* *** $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     00001900
