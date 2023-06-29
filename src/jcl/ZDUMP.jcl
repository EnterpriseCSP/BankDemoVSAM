//MFIDEMO  JOB MFIDEMO,MFIDEMO,CLASS=A,MSGCLASS=A,NOTIFY=MFIDEMO        00000100
//* ******************************************************************  00000200
//* ZBNKINT2.JCL                                                        00000300
//* ******************************************************************  00000400
//* STEP01 - DEFINE VSAM FILES REQUIRED IF USING CICS                   00000500
//* ******************************************************************  00000600
//*DEL      EXEC PGM=IEFBR14                                            00000700
//*DD1      DD DSN=MFI01.MFIDEMO.PC.BNKATYPE.SEQ,                       00000800
//*            DISP=(MOD,DELETE,DELETE),                                00000900
//*            UNIT=SYSDA,SPACE=(TRK,(2,1),RLSE)                        00001000
//STEP01   EXEC PGM=IDCAMS                                              00001100
//SYSPRINT DD SYSOUT=*                                                  00001200
//SYSIN    DD *                                                         00001300
  PRINT IDS(MFI01V.MFIDEMO.PC.BNKATYPE)                                 00001400
//*BNKATYPE  DD DSN=MFI01.MFIDEMO.PC.BNKATYPE.SEQ,                      00001400
//*            DISP=(NEW,CATLG),                                        00001500
//*            UNIT=SYSDA,SPACE=(TRK,(2,1),RLSE)                        00001600
//*                                                                     00001700
//* *** $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     00001800
