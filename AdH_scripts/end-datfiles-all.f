      PROGRAM ADHENDDAT 
      PARAMETER (MAXNE = 500000, MAXN = 1000000, MAXNDF = 6)
      INTEGER NPTOT, NETOT,  NN, ITIME, NFILE, columns
      REAL SAVE_TM, TIME, value_col(20) 
      CHARACTER filename*80,filename2*80,junk1*2,junk2*2,variable*8 
      CHARACTER filename3*80, EXT*8

      WRITE (*,*) 'enter the input filename'
      READ (*,'(A)')filename

      OPEN(15,file=filename,form='formatted',status='old')
      REWIND(15)
      
      READ (15,*) SAVE_TM 
      READ (15,*) NFILE
      DO IFILE = 1,NFILE
        READ (15,*) filename2, columns 
        WRITE (*,*) filename2,columns 
        OPEN (20,file=filename2,form='formatted',status='old')
        REWIND (20)
        READ (20,*) 
        READ (20,*)
        READ (20,'(A)')variable 
        READ (20,*) junk1, NPTOT
        READ (20,*) junk2, NETOT
        READ (20,*)
        WRITE (EXT,1000) SAVE_TM
        WRITE (*,*) EXT
        IL = index(filename2,' ')-1 
        filename3=filename2(1:IL)//'_'//EXT(1:8)  
        WRITE (*,*) filename3 
        OPEN(68,file=filename3,form='formatted',status='unknown')
c       OPEN(68,file='test1',form='formatted',status='unknown')
        WRITE(68,*)'DATASET' 
        WRITE(68,*)'OBJTYPE "mesh2d"' 
        WRITE(68,*)variable
        WRITE(68,*)'ND ', NPTOT
        WRITE(68,*)'NC ', NETOT
        WRITE(68,*)'NAME ', filename2 

        DO ITIME = 1, 9999999
          READ(20,*,END=900) junk1, junk2, TIME
          IF(TIME .LE. SAVE_TM) THEN
            WRITE(68,*) 'TS  0  ', TIME 
            
              DO NN=1,NPTOT
                READ(20,*,END=900)(value_col(I),I=1,columns)
c               WRITE(68,*)(value_col(I),I=1,columns)
                WRITE(68,950)(value_col(I),I=1,columns)
              ENDDO
          ELSE
           GOTO 900
          ENDIF
        ENDDO
  900   CONTINUE
      WRITE (68,*)'ENDDS' 
      CLOSE (20)
      CLOSE (68) 
      ENDDO
  950 FORMAT(20E15.6)
 1000 FORMAT(F8.2)
      CLOSE(15)
      END
