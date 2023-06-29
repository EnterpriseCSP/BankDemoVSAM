000100***************************************************************** chelpvsm
000200*                                                               * chelpvsm
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * chelpvsm
000400*                                                               * chelpvsm
000500***************************************************************** chelpvsm
000600                                                                  chelpvsm
000700***************************************************************** chelpvsm
000800* CHELPVSM.CPY                                                  * chelpvsm
000900*---------------------------------------------------------------* chelpvsm
001000* This is the record file record layout for help records        * chelpvsm
001100***************************************************************** chelpvsm
001200   05  HLP-RECORD                            PIC X(83).           chelpvsm
001300   05  FILLER REDEFINES HLP-RECORD.                               chelpvsm
001400     10  HLP-KEY.                                                 chelpvsm
001500       15  HLP-SCRN                          PIC X(6).            chelpvsm
001600       15  HLP-LINE                          PIC X(2).            chelpvsm
001700     10  HLP-TEXT                            PIC X(75).           chelpvsm
001800                                                                  chelpvsm
001900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     chelpvsm
