//MFIDEMO  JOB MFIDEMO,MFIDEMO,CLASS=1,MSGCLASS=A,NOTIFY=MFIDEMO        00000100
//STEP04   EXEC PGM=IDCAMS                                              00000200
//SYSPRINT DD SYSOUT=X                                                  00000300
//SYSIN    DD *                                                         00000400
  LISTC  LVL     (MFI01V.MFIDEMO)                                       00000500
//*                                                                     00000500
//* *** $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     00000600
