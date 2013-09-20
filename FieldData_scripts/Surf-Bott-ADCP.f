       PROGRAM SurfBottADCP

       REAL X(2000),Y(2000),Z(2000)
       REAL Junk(2000), Junk2(2000)
       REAL Vx(2000), Vy(2000)
       REAL Surf_X, Surf_Y, Surf_Z, Surf_Vx, Surf_Vy
       REAL Bott_X, Bott_Y, Bott_Z, Bott_Vx, Bott_Vy
       CHARACTER filename*80
       INTEGER i


       WRITE(*,*) 'Enter the name of the file with the ADCP data.'
       READ(*,'(a)') filename
       OPEN(10,file=filename,form='formatted',status='old') 
       REWIND(10)
       OPEN(20,file='Surf_'//filename,form='formatted',status='unknown') 
       OPEN(25,file='Bott_'//filename,form='formatted',status='unknown') 

       DO i=1,999999 
         READ(10,end=300,*) X(i), Y(i), Z(i), Junk(i), Junk2(i),
     # Vx(i), Vy(i)
         READ(10,end=300,*) X(i+1), Y(i+1), Z(i+1), Junk(i+1), 
     # Junk2(i+1), Vx(i+1), Vy(i+1)
         write(*,*) 'I = ', i
         IF(i.EQ.1) THEN
           Surf_X=X(i)
           Surf_Y=Y(i)
           Surf_Z=Z(i)
           Surf_Vx=Vx(i)
           Surf_Vy=Vy(i)
           WRITE(20,400) Surf_X, Surf_Y, Surf_Z, Surf_Vx, Surf_Vy
         ENDIF
         IF(X(i+1).NE.X(i) .AND. Y(i+1).NE.Y(i)) THEN 
           Bott_X=X(i)
           Bott_Y=Y(i)
           Bott_Z=Z(i)
           Bott_Vx=Vx(i)
           Bott_Vy=Vy(i)
           Surf_X=X(i+1)
           Surf_Y=Y(i+1)
           Surf_Z=Z(i+1)
           Surf_Vx=Vx(i+1)
           Surf_Vy=Vy(i+1)
           WRITE(20,400) Surf_X, Surf_Y, Surf_Z, Surf_Vx, Surf_Vy
           WRITE(25,400) Bott_X, Bott_Y, Bott_Z, Bott_Vx, Bott_Vy
         ENDIF

         Backspace(10)

       ENDDO
300    CONTINUE

           Bott_X=X(i)
           Bott_Y=Y(i)
           Bott_Z=Z(i)
           Bott_Vx=Vx(i)
           Bott_Vy=Vy(i)
           WRITE(25,400) Bott_X, Bott_Y, Bott_Z, Bott_Vx, Bott_Vy

400    FORMAT(5F14.2)

       CLOSE(10)
       CLOSE(20)
       CLOSE(25)

       END
            
         
