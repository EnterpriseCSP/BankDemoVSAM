//MFIDEMO  JOB MFIDEMO,MFIDEMO,CLASS=A,MSGCLASS=A,NOTIFY=MFIDEMO        00000100
//* ******************************************************************  00000200
//* ZBNKINT1                                                            00000300
//* ******************************************************************  00000400
//* STEP01 - DEFINE REQUIRED GENERATION DATA GROUPS (GDG)               00000500
//* ******************************************************************  00000600
//STEP01  EXEC PGM=IDCAMS                                               00000700
//SYSPRINT DD SYSOUT=*                                                  00000800
//SYSIN    DD *                                                         00000900
  DELETE MFI01.MFIDEMO.BANKSRT1            -                            
         FORCE                                                          
  DEFINE GDG (NAME(MFI01.MFIDEMO.BANKSRT1) -                            
              LIMIT(3)                     -                            
              SCRATCH)                                                  
  DELETE MFI01.MFIDEMO.BANKSRT2            -                            
         FORCE                                                          
  DEFINE GDG (NAME(MFI01.MFIDEMO.BANKSRT2) -                            
              LIMIT(3)                     -                            
              SCRATCH)                                                  
                                                                        
  DELETE MFI01.MFIDEMO.BANKEXT1            -                            
         FORCE                                                          
  DEFINE GDG (NAME(MFI01.MFIDEMO.BANKEXT1) -                            
              LIMIT(3)                     -                            
              SCRATCH)                                                  
  DELETE MFI01.MFIDEMO.BANKEXT2            -                            
          FORCE                                                         
  DEFINE GDG (NAME(MFI01.MFIDEMO.BANKEXT2) -                            
              LIMIT(3)                     -                            
              SCRATCH)                                                  
  SET    MAXCC=0                                                        
  LISTC  LVL     (MFI01.MFIDEMO)                                        
//*                                                                     00001000
//* *** $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     00001100
