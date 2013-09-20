       PROGRAM COMPUTE_MAX_AVG

       PARAMETER (MAXT=1000000, MAXP=100)
  
       INTEGER points, i, j, n
       REAL VAL(MAXP), COUNT, MAX_POS(MAXP), MAX_NEG(MAXP)
       REAL AVG_POS(MAXP), AVG_NEG(MAXP)
       CHARACTER filename*80

       WRITE(*,*) 'enter the filename with the data to be analyzed'
       READ(*,'(a)') filename

       WRITE(*,*) 'Enter the number of points in the file'
       READ(*,*) points

       DO i=1,points
         MAX_NEG(i)=0.0
         MAX_POS(i)=0.0
         AVG_POS(i)=0.0
         AVG_NEG(i)=0.0
       ENDDO

       COUNT=0.0

       OPEN(15,file=filename,form='formatted',status='old')
       REWIND(15)


       DO j=1,MAXT
         READ(15,*,END=100) (VAL(n),n=1,points)
          DO i=1,points
             IF(VAL(i).LT.0.0) THEN
               AVG_NEG(i)=AVG_NEG(i)+VAL(i) 
             ELSE
               AVG_POS(i)=AVG_POS(i)+VAL(i)
             ENDIF

             IF(VAL(i).GT.MAX_POS(i)) THEN
               MAX_POS(i) = VAL(i)
             ENDIF

             IF(VAL(i).LT.MAX_NEG(i)) THEN
               MAX_NEG(i) = VAL(i)
             ENDIF
  

           ENDDO

 
           COUNT = COUNT+1.0  

       ENDDO

100    CONTINUE

       DO i=1,points
          AVG_POS(i) = AVG_POS(i)/COUNT
          AVG_NEG(i) = AVG_NEG(i)/COUNT
       ENDDO

       OPEN(25,file='OUT',form='formatted',status='unknown')
       WRITE(25,'(a)') 'POINT   MAX_POS   MAX_NEG   AVG_POS   AVG_NEG'
       DO i=1,points
          WRITE(25,200) i,MAX_POS(i),MAX_NEG(i),AVG_POS(i),AVG_NEG(i)
       ENDDO 

200    FORMAT(I8, 4F10.6)

       CLOSE(15)
       CLOSE(25)

       END
       
