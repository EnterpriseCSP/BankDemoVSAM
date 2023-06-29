//MFIDEMO  JOB MFIDEMO,MFIDEMO,CLASS=A,MSGCLASS=A,MSGLEVEL=(2,0),       00000100
//  NOTIFY=MFIDEMO                                                      00000200
//EXTRACT  EXEC YBNKEXTV,REQUEST=B0004                                  00000300
//EXTRACT.SYSOUT DD DUMMY                                               00000400
//SORT     EXEC YBNKSRT1,GEN='+1'                                       00000500
//SORT.SYSOUT DD DUMMY                                                  00000600
//PRINT    EXEC YBNKPRT1,GEN='+1'                                       00000700
//*                                                                     00000800
//* *** $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     00000900
