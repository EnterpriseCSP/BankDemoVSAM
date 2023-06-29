//MFIDEMO  JOB MFIDEMO,MFIDEMO,CLASS=A,MSGCLASS=A,NOTIFY=MFIDEMO        00000100
//* ******************************************************************  00000200
//* ZBNKPRT1.JCL                                                        00000300
//* ******************************************************************  00000400
//PRINT    EXEC PGM=ZBNKPRT1,PARM='HELLO'                               00000500
//EXTRACT  DD DSN=MFI01.MFIDEMO.BANKSRT1(0),DISP=SHR                    00000700
//PRINTOUT DD SYSOUT=*                                                  00000800
//SYSOUT   DD SYSOUT=*