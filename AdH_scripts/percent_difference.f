      PROGRAM PERCENTDIFF 
      REAL TIMEF, TIMEM, VALF, VALM
      CHARACTER filename*80, filenameF*80, filenameM*80



      WRITE(*,*) 'enter the input field data text file' 
      READ(*,'(A)') filenameF
      OPEN(10,file=filenameF,form='formatted',status='unknown')
      WRITE(*,*) 'enter the input model data text file' 
      READ(*,'(A)') filenameM
      OPEN(12,file=filenameM,form='formatted',status='unknown')
      WRITE(*,*) 'enter the output data filename'
      READ(*,'(A)') filename
      OPEN(15,file=filename,form='formatted',status='unknown')
      REWIND(15)

      WRITE(15,*)'Percent Difference for each station' 
      WRITE(15,*)'Field Time     Field Val     Model Val     % Diff' 

200   READ(10,*,END=400) TIMEF, VALF
      READ(12,*, END=400) TIMEM, VALM
      IF(TIMEF .LE. TIMEM) THEN
        WRITE(15,*) TIMEF, VALF, VALM, (VALM-VALF)/VALF*100.0 
      ELSE
        GOTO 200
      ENDIF
    

400   CLOSE(15)
500   CLOSE(12)
600   CLOSE(10)

      END
