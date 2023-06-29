000100***************************************************************** csetuib
000200*                                                               * csetuib
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * csetuib
000400*                                                               * csetuib
000500***************************************************************** csetuib
000600                                                                  csetuib
000700***************************************************************** csetuib
000800* CSETUIB.CPY (CICS Version)                                    * csetuib
000900*---------------------------------------------------------------* csetuib
001000* This copybook is used to call CBLTDLI to set the address of   * csetuib
001100* the DLIUIB (User Interface Block). This needs to be done when * csetuib
001200* CICS is accessing DL/I databases.                             * csetuib
001300* This does not have to be done in an IMS environment.          * csetuib
001400* There are different versions for CICS, IMS and INET.          * csetuib
001500***************************************************************** csetuib
001600     CALL 'CBLTDLI' USING WS-IMS-PCB                              csetuib
001700                          WS-PSB-NAME                             csetuib
001800                          ADDRESS OF DLIUIB.                      csetuib
001900                                                                  csetuib
002000* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     csetuib
