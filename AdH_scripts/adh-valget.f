      PROGRAM ADHVALGET 
      DIMENSION PT(100000)
      INTEGER NPTOT, NETOT,  NN, ITIME, NPTS, PTS, PT
      REAL DEPTH, VALUE, TIME
      CHARACTER filename*80,filename2*80,junk1*2,junk2*2,filename1*20
      CHARACTER junk3*9, TUNITS*20


      WRITE(*,*) 'enter the ADH single column solution filename'
      READ(*,'(A)') filename
      OPEN(15,file=filename,form='formatted',status='old')
      REWIND(15)
      READ(15,*)
      READ(15,*)
      READ(15,*)
      READ(15,*) junk1, NPTOT
      READ(15,*) junk2, NETOT 
      READ(15,*) 
      READ(15,*) junk3, TUNITS

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
              READ(15,*,END=900) VALUE
               IF(NN .EQ. PT(NPTS))THEN
                 WRITE(68,*) NN, TIME, VALUE 
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
        READ(15,*) junk3, TUNITS
      ENDDO
        WRITE(68,*)'ENDDS' 
        CLOSE(15)
        CLOSE(20)
        CLOSE(68)

      END
