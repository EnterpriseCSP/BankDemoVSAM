000100***************************************************************** ctrace
000200*                                                               * ctrace
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ctrace
000400*                                                               * ctrace
000500***************************************************************** ctrace
000600                                                                  ctrace
000700***************************************************************** ctrace
000800* CTRACE.CPY                                                    * ctrace
000900*---------------------------------------------------------------* ctrace
001000* This copybook is used to provide an a trace of what           * ctrace
001100* transactions have been run so we get an idea of activity      * ctrace
001200* There are different versions for CICS and IMS.                * ctrace
001300***************************************************************** ctrace
001400*                                                                 ctrace
001500* Comment out the instructions and recompile to not use the trace ctrace
001600     EXEC CICS LINK PROGRAM('STRAC00P')                           ctrace
001700                    COMMAREA(WS-PROGRAM-ID)                       ctrace
001800                    LENGTH(LENGTH OF WS-PROGRAM-ID)               ctrace
001900    END-EXEC.                                                     ctrace
002000                                                                  ctrace
002100* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ctrace
