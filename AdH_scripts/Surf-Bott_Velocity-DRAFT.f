      PROGRAM SURFBOTVEL 
      PARAMETER (MAXNE = 500000, MAXN = 1000000, MAXNDF = 6)
      INTEGER NPTOT, NETOT, N, NN, ITIME
      REAL c_ref,COEF,U_R,V_MAG,AVE_X,AVE_Y,BOT_VEL_X,BOT_VEL_Y,ZERO
      REAL VALX,VALY,VALZ,DEP,VOR,ER_X,ER_Y,SUR_VEL_X,SUR_VEL_Y
      REAL VS, VB, k, draft, VD, U_RD,AVG_VEL_X, AVG_VEL_Y
      CHARACTER filename1*80,dep_file*80,ovl_file*80,vor_file*80
      CHARACTER junk1*2,junk2*2, VAR*3, filename2*80, filename3*80


      WRITE(*,*) 'This routine will calculate the surface and bottom'
      WRITE(*,*) 'velocities based on the depth average and vorticity.'
      WRITE(*,*) 'Requires the depth, velocity, and vorticity files'
      WRITE(*,*) 'enter the output filename for surface vel'
      READ(*,'(A)') filename1
      OPEN(68,file=filename1,form='formatted',status='unknown')
      WRITE(*,*) 'enter the output filename for bottom vel'
      READ(*,'(A)') filename2
      OPEN(69,file=filename2,form='formatted',status='unknown')

      WRITE(*,*) 'what is the depth filename?' 
      READ(*,'(A)') dep_file 
        OPEN(15,file=dep_file,form='formatted',status='old')
      WRITE(*,*) 'what is the velocity filename?' 
      READ(*,'(A)') ovl_file 
        OPEN(16,file=ovl_file,form='formatted',status='old')
      WRITE(*,*) 'what is the vorticity filename?' 
      WRITE(*,*) 'enter "NULL" if there is no vorticity.' 
      READ(*,'(A)') vor_file 
        IF(vor_file .NE. 'NULL') THEN
          OPEN(17,file=vor_file,form='formatted',status='old')
        ENDIF
      WRITE(*,*) 'what is the roughness height ?' 
      WRITE(*,*) 'k = (25.84*n)^6'  
      READ(*,*) k

      WRITE(*,*) 'Enter the vessel draft.'
	WRITE(*,*) 'Average velocity will be from the surface to this draft.'
      WRITE(*,*) 'If you only want surface and bottom enter 0.'
      READ(*,*) draft
      WRITE(*,*) 'Enter the filename for the draft average velocity.'
      READ(*,'(A)') filename3
        IF(draft .NE. 0.0) THEN
          OPEN(70,file=filename3,form='formatted',status='unknown')
        ENDIF

      c_ref=5.0


        REWIND(15)
        READ(15,*)
        READ(15,*)
        READ(15,*)
        READ(15,*) junk1, NPTOT
        READ(15,*) junk2, NETOT 
        READ(15,*)  
        READ(15,*)  

        REWIND(16)
        READ(16,*)
        READ(16,*)
        READ(16,*)
        READ(16,*) junk1, NPTOT
        READ(16,*) junk2, NETOT 
        READ(16,*)  
        READ(16,*)  

       IF(vor_file .NE. 'NULL') THEN
        REWIND(17)
        READ(17,*)
        READ(17,*)
        READ(17,*)
        READ(17,*) junk1, NPTOT
        READ(17,*) junk2, NETOT 
        READ(17,*) 
        READ(17,*) 
       ENDIF

        ZERO = 0.0

                WRITE(68,*) 'DATASET'
                WRITE(68,*) 'OBJTYPE "mesh2d"'
                WRITE(68,*) 'BEGVEC'
                WRITE(68,*) 'ND  ', NPTOT
                WRITE(68,*) 'NC  ', NETOT
                WRITE(68,*) 'NAME  "Surface Velocity"'

                WRITE(69,*) 'DATASET'
                WRITE(69,*) 'OBJTYPE "mesh2d"'
                WRITE(69,*) 'BEGVEC'
                WRITE(69,*) 'ND  ', NPTOT
                WRITE(69,*) 'NC  ', NETOT
                WRITE(69,*) 'NAME  "Bottom Velocity"'

                WRITE(70,*) 'DATASET'
                WRITE(70,*) 'OBJTYPE "mesh2d"'
                WRITE(70,*) 'BEGVEC'
                WRITE(70,*) 'ND  ', NPTOT
                WRITE(70,*) 'NC  ', NETOT
                WRITE(70,*) 'NAME  "Draft Average Velocity"'



        DO ITIME = 1, 9999999
          READ(15,*,END=900) junk1, junk2, TIME1
          READ(16,*,END=900) junk1, junk2, TIME2
          IF(vor_file .NE. 'NULL') THEN
            READ(17,*,END=900) junk1, junk2, TIME2
          ENDIF
                WRITE(68,*) 'TS  0  ', TIME1
                WRITE(69,*) 'TS  0  ', TIME1
                WRITE(70,*) 'TS  0  ', TIME1
              DO N=1,NPTOT
                READ(15,*,END=900) DEP
                IF(vor_file .NE. 'NULL') THEN
                  READ(17,*,END=900) VOR
                ELSE
                  VOR = 0.0
                ENDIF
                READ(16,*,END=900) VALX, VALY, VALZ
                V_MAG=SQRT(VALX*VALX+VALY*VALY)
                IF (V_MAG .GT. 0.0) THEN
                  AVE_X=VALX/V_MAG
                  AVE_Y=VALY/V_MAG
                ELSE
                  AVE_X = 0.0
                  AVE_Y = 0.0
                ENDIF
                ER_X=-1.*AVE_Y
                ER_Y=AVE_X
                COEF=SQRT(6.* c_ref)
                U_R=(6.*VOR*DEP)/COEF
                U_RD=(6.*VOR*(DEP-draft))/COEF
               
                IF (DEP .LE. 0.0) THEN
                  VB = 0.0
                  VS = 0.0
                ELSE
                  VB = LOG(0.05*29.7*DEP/k+1.0)
                  VB = VB/(LOG(0.368*29.7*DEP/k+1.0))
                  VS = LOG(29.7*DEP/k+1.0)
                  VS = VS/(LOG(0.368*29.7*DEP/k+1.0))
                  IF(DEP .LE. draft) THEN
                    VD = VS
                  ELSE 
                    VD = LOG((DEP-draft/2.)*29.7/k+1.0)
                    VD = VD/(LOG(0.368*29.7*DEP/k+1.0))
                  ENDIF  
                ENDIF

                BOT_VEL_X=U_R*ER_X + VB*VALX
                BOT_VEL_Y=U_R*ER_Y + VB*VALY

                SUR_VEL_X=U_R*-1.*ER_X + VS*VALX
                SUR_VEL_Y=U_R*-1.*ER_Y + VS*VALY

                AVG_VEL_X=0.5*((U_RD*-1.*ER_X+VD*VALX)+
     #            (U_R*-1.*ER_X+VS*VALX))
                AVG_VEL_Y=0.5*((U_RD*-1.*ER_Y+VD*VALY)+
     #            (U_R*-1.*ER_Y+VS*VALY))

                WRITE(68,*) SUR_VEL_X,  SUR_VEL_Y, ZERO
                WRITE(69,*) BOT_VEL_X,  BOT_VEL_Y, ZERO
                WRITE(70,*) AVG_VEL_X,  AVG_VEL_Y, ZERO
              ENDDO
        ENDDO
  900   CONTINUE
                WRITE(68,*) 'ENDDS' 
                WRITE(69,*) 'ENDDS'
                WRITE(70,*) 'ENDDS'
        CLOSE(15)
        CLOSE(16)
        CLOSE(17)
  950   FORMAT(A3,3X,I8,3F18.6,F8.4,4F12.6)
        CLOSE(68)
        CLOSE(69)
        CLOSE(70)

      END
