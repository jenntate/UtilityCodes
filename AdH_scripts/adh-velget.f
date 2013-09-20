      PROGRAM ADHVELGET 
      DIMENSION PT(100000)
      INTEGER NPTOT, NETOT,  NN, ITIME, NPTS, PTS, PT
      REAL VALUE, TIME, VALX, VALY, VALZ
      CHARACTER filename*80,filename2*80,junk1*2,junk2*2,filename1*20



      WRITE(*,*) 'enter the ADH velocity solution filename'
      READ(*,'(A)') filename
      OPEN(15,file=filename,form='formatted',status='old')
      REWIND(15)
      READ(15,*)
      READ(15,*)
      READ(15,*)
      READ(15,*) junk1, NPTOT
      READ(15,*) junk2, NETOT 
      READ(15,*) 

	WRITE(*,*) 'enter the file with the desired nodes'
        READ(*,'(A)') filename1
        OPEN(20,file=filename1,form='formatted',status='old')
	READ(20, *) PTS
	DO NPTS = 1, PTS
        READ(20,*) PT(NPTS)
      ENDDO 

      
      WRITE(*,*) 'enter the desired ADH ouput solution filename'
      READ(*,'(A)') filename2
      OPEN(68,file=filename2,form='formatted',status='unknown')
      WRITE(68, *) '  NODE       TIME           VALUE     '

      DO NPTS=1, PTS
        DO ITIME = 1, 9999999
          READ(15,*,END=900) junk1, junk2, TIME
            DO NN=1,NPTOT
              READ(15,*,END=900) VALX, VALY, VALZ
               IF(NN .EQ. PT(NPTS))THEN
                 WRITE(68,*) NN, TIME, VALX, VALY, VALZ 
               ENDIF
            ENDDO
         ENDDO
  900   CONTINUE
         REWIND(15)
         READ(15,*)
         READ(15,*)
         READ(15,*)
         READ(15,*) junk1, NPTOT
         READ(15,*) junk2, NETOT 
         READ(15,*) 
       ENDDO
        WRITE(68,*)'ENDDS' 
        CLOSE(15)
        CLOSE(20)
        CLOSE(68)

      END
