       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. TESTSEC1.                                             
      *****************************************************************    
       DATA DIVISION.                                                      
       WORKING-STORAGE SECTION.                                            
        01 cvda-rtn                    pic s9(8)    comp.
           88 READABLE                   value 35.
           88 NOTREADABLE                value 36.
           88 UPDATABLE                  value 37.
           88 NOUPDATABLE                value 38.
           88 ALTERABLE                  value 52.
           88 NOTALTERABLE               value 53.
           88 CONTROLABLE                value 56.
           88 NOTCONTROLABLE             value 57.

       01  wcomf-neste-trans-x4    pic x(4) value spaces.
       01  wcomf-neste-trans-x12   pic x(12) value spaces.
                                                                           
       PROCEDURE DIVISION.                                                      

      *    move 'TRN1'   to wcomf-neste-trans-x4 wcomf-neste-trans-x12.
           move 'HPC1'   to wcomf-neste-trans-x4 wcomf-neste-trans-x12.

           EXEC CICS QUERY SECURITY 
             RESTYPE        ('TRANSACTION') 
             RESID          (WCOMF-NESTE-TRANS-x4) 
             READ           (cvda-rtn) 
           END-EXEC.

           If READABLE
              display 'Readable from x4 is true'
           Else
              display 'Readable from x4 is not true'
           End-If.

           EXEC CICS QUERY SECURITY 
             RESTYPE        ('TRANSACTION') 
             RESID          (WCOMF-NESTE-TRANS-x12) 
             READ           (cvda-rtn) 
           END-EXEC.

           If READABLE
              display 'Readable from x12 is true'
           Else
              display 'Readable from x12 is not true'
           End-If.

          EXEC CICS QUERY SECURITY 
             RESTYPE        ('TRANSACTION') 
             RESID          (WCOMF-NESTE-TRANS-x4) 
             CONTROL        (cvda-rtn) 
           END-EXEC.

           If CONTROLABLE
              display 'Controlable from x4 is true'
           Else
              display 'Controlable from x4 is not true'
           End-If.

           EXEC CICS QUERY SECURITY 
             RESTYPE        ('TRANSACTION') 
             RESID          (WCOMF-NESTE-TRANS-x12) 
             CONTROL        (cvda-rtn) 
           END-EXEC.

           If CONTROLABLE
              display 'Controlable from x12 is true'
           Else
              display 'Controlable from x12 is not true'
           End-If.

          EXEC CICS QUERY SECURITY 
             RESTYPE        ('TRANSACTION') 
             RESID          (WCOMF-NESTE-TRANS-x4) 
             ALTER          (cvda-rtn) 
           END-EXEC.

           If ALTERABLE
              display 'Alterable from x4 is true'
           Else
              display 'Alterable from x4 is not true'
           End-If.

           EXEC CICS QUERY SECURITY 
             RESTYPE        ('TRANSACTION') 
             RESID          (WCOMF-NESTE-TRANS-x12) 
             ALTER          (cvda-rtn) 
           END-EXEC.

           If ALTERABLE
              display 'Alterable from x12 is true'
           Else
              display 'Alterable from x12 is not true'
           End-If.

          EXEC CICS QUERY SECURITY 
             RESTYPE        ('TRANSACTION') 
             RESID          (WCOMF-NESTE-TRANS-x4) 
             UPDATE         (cvda-rtn) 
           END-EXEC.

           If UPDATABLE
              display 'Updatable from x4 is true'
           Else
              display 'Updatable from x4 is not true'
           End-If.

           EXEC CICS QUERY SECURITY 
             RESTYPE        ('TRANSACTION') 
             RESID          (WCOMF-NESTE-TRANS-x12) 
             UPDATE         (cvda-rtn) 
           END-EXEC.

           If UPDATABLE
              display 'Updatable from x12 is true'
           Else
              display 'Updatable from x12 is not true'
           End-If.
           
           EXEC CICS RETURN END-EXEC.
        
		   GOBACK.
