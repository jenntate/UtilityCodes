      PROGRAM RMA10SMS 
      PARAMETER (MAXNE = 200000, MAXN = 200000, MAXNDF = 10)

      DIMENSION NOP(MAXNE,20), NDEP(MAXN), NREF(MAXN), CORD(MAXN,3),
     &          XVEL(MAXNDF,MAXN), AO(MAXN), DELBED(MAXN), BSHR(MAXN)
      DIMENSION VEL(MAXNDF,MAXN), VVEL(MAXN), WSEL(MAXN), NDRY(MAXN),
     &          IMAT(MAXNE), NSURF(MAXN), IRESAV(9)
      DIMENSION ICNT(MAXN),NODEVS(30),NBOT(MAXN)
      DIMENSION SSAVE(MAXN),BSAVE(MAXN)
      DIMENSION SVAVE(3,MAXN),BVAVE(3,MAXN)
      INTEGER   IMON(1000000),IYR(1000000)

      CHARACTER filename*80,CHND*6,CDATE*7

      DATA LUSVEL /8/, LUBVEL /9/, LUSSAL /10/, LUBSAL /11/

      NQAL = 1
      DO I = 1,MAXN
        DO J = 1,MAXNDF
          XVEL(J,I) = 0.0
        END DO
        DELBED(I) = 0.0
        BSHR(I) = 0.0
        SSAVE(I) = 0.0
        BSAVE(I) = 0.0
        SVAVE(1,I) = 0.0
        SVAVE(2,I) = 0.0
        SVAVE(3,I) = 0.0
        BVAVE(1,I) = 0.0
        BVAVE(2,I) = 0.0
        BVAVE(3,I) = 0.0
      END DO
      IC=0
      OPEN(14,file='zdate.out',form='formatted',status='old')
      REWIND(14)
  221 IC=IC+1
      READ(14,'(I2,4X,I4)',END=222) IMON(IC),IYR(IC)
      GOTO 221
  222 CONTINUE
      IC=IC-1
      WRITE(*,*) "NUMBER OF DATES FOUND: ",IC
      CLOSE(14)

      WRITE(*,*) 'enter the rma10 3d-binary geometry file name'
      READ(*,'(A)') filename
      OPEN(14,file=filename,form='unformatted',status='old')
      REWIND(14)
      READ (14,ERR=900) NPTOT, NETOT, NP, NE,
     *  ((CORD(J,K),SPECDUM,K=1,3),ADUM,NXDUM,AO(J),NSURF(J),J=1,NPTOT), 
     *  (NDEP(J),NREF(J),J=1,NP),((NOP(J,K),K=1,20),NCDUM,IMAT(J),
     *  THDUM, NXHDUM,J=1,NETOT),(WIDUM,J=1,NPTOT)
      CLOSE(14)
      WRITE(*,*) "   2D Geometry Nodes: ",NP
      WRITE(*,*) "   3D Geometry Nodes: ",NPTOT
      WRITE(*,*) "2D Geometry Elements: ",NE
      WRITE(*,*) "3D Geometry Elements: ",NETOT

      DO I = 1, NP
        NBOT(I) = I
        IF (NREF(I) .GT. 0) NBOT(I) = NREF(I) + NDEP(I) - 1
      ENDDO

      WRITE(*,*) 'enter the rma10 binary solution file name'
      READ(*,'(A)') filename
      OPEN(15,file=filename,form='unformatted',status='old')
      REWIND(15)

      WRITE(*,*) 'Enter ascii file with node numbers to be extracted'
      WRITE(*,*) '(note: this file should consist of the following:'
      WRITE(*,*) ' an integer n, followed by n node numbers (30 max))' 
      READ(*,'(A)') filename
      NNODES=0
      IF(filename .NE. '') THEN
         OPEN(16,file=filename,form='formatted',status='old')
         REWIND(16)
         READ(16,*) NNODES,(NODEVS(I),I=1,NNODES)
         CLOSE(16)
      ENDIF
      IF(NNODES.GT.30) THEN
        WRITE(*,*) 'Too many nodes listed in nodes file (Max=20).'
        STOP
      ENDIF

      IF(NNODES.GT.0) THEN
        DO I=1,NNODES
          IF(NODEVS(I).GT.NP) THEN
             WRITE(*,*) 'Node number listed in node file greater than ',
     &                  'number of nodes in 2D geometry.'
             WRITE(*,*) 'Execution Stopped!'
             STOP
          ENDIF
        ENDDO
        DO I=1,NNODES
          INM=20+I
          WRITE(CHND,'(I6.6)') NODEVS(I)
          filename='NODALVEL--'//CHND
          OPEN(INM,file=filename,form='formatted',status='unknown')
          REWIND(INM)
          WRITE(INM,*) 'VELOCITIES FOR NODE ',NODEVS(I),
     &                 '(',NBOT(NODEVS(I)),')'
          WRITE(INM,*) 
          WRITE(INM,2000) 'TIME','SURF-VEL-X','SURF-VEL-Y',
     &                           'BOTT-VEL-X','BOTT-VEL-Y'
 2000     FORMAT(A9,2X,A12,A12,A12,A12)
          INM=50+I
          filename='NODALSAL--'//CHND
          OPEN(INM,file=filename,form='formatted',status='unknown')
          REWIND(INM)
          WRITE(INM,*) 'SALINITIES FOR NODE ',NODEVS(I),
     &                 '(',NBOT(NODEVS(I)),')'
          WRITE(INM,*) 
          WRITE(INM,2100) 'TIME','SURF-SAL','BOTT-SAL'
 2100     FORMAT(A9,2X,A12,A12)
        ENDDO
      ENDIF
 
      WRITE(CDATE,'(I2.2,"-",I4.4)') IMON(1),IYR(1)
      FILENAME='SVEL_'//CDATE//'.sol'
      OPEN(LUSVEL,FILE=FILENAME,FORM='UNFORMATTED', STATUS='UNKNOWN')
      REWIND(LUSVEL)
      CALL PUT_BANNER (NE, NP, LUSVEL, 2)
      FILENAME='BVEL_'//CDATE//'.sol'
      OPEN(LUBVEL,FILE=FILENAME,FORM='UNFORMATTED', STATUS='UNKNOWN')
      REWIND(LUBVEL)
      CALL PUT_BANNER (NE, NP, LUBVEL, 2)
      FILENAME='SSAL_'//CDATE//'.sol'
      OPEN(LUSSAL,FILE=FILENAME,FORM='UNFORMATTED', STATUS='UNKNOWN')
      REWIND(LUSSAL)
      CALL PUT_BANNER (NE, NP, LUSSAL, 4)
      FILENAME='BSAL_'//CDATE//'.sol'
      OPEN(LUBSAL,FILE=FILENAME,FORM='UNFORMATTED', STATUS='UNKNOWN')
      REWIND(LUBSAL)
      CALL PUT_BANNER (NE, NP, LUBSAL, 4)

      COUNT=0.0
      IMON_LST=IMON(1)
      IYR_LST=IYR(1)
      DO ITIME = 1, 1000000
        READ (15,END=900) TIME, NP3, NDF, NE1,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((XVEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMAT(J), J = 1, NE1), (NDRY(J), J = 1, NP3),
     &       (DELBED(J),J=1,IRESAV(7)),(BSHR(J),J=1,IRESAV(NDFS)),
     &       (VVEL(J), J = 1, NP3), (DUM, J = 1, NE1)

        IF(IMON_LST .NE. IMON(ITIME)) THEN
          IF(COUNT.GT.0.0) THEN
            WRITE(*,*) "MON,YR,COUNT ",IMON_LST,IYR_LST,COUNT
            DO I=1,NP
              SSAVE(I)=SSAVE(I)/COUNT
              BSAVE(I)=BSAVE(I)/COUNT
              SVAVE(1,I)=SVAVE(1,I)/COUNT
              SVAVE(2,I)=SVAVE(2,I)/COUNT
              SVAVE(3,I)=SVAVE(3,I)/COUNT
              BVAVE(1,I)=BVAVE(1,I)/COUNT
              BVAVE(2,I)=BVAVE(2,I)/COUNT
              BVAVE(3,I)=BVAVE(3,I)/COUNT
            ENDDO
            LU=14
            WRITE(CDATE,'(I2.2,"-",I4.4)') IMON_LST,IYR_LST
            FILENAME='SSALave_'//CDATE//'.sol'
            OPEN(LU,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
            REWIND(LU)
            CALL PUT_BANNER (NE, NP, LU, 4)
            WRITE(LU) 1.0,NQAL,NP,(SSAVE(K),K=1,NP),NE,(IMAT(K),K=1,NE)
            CLOSE(LU)
            FILENAME='BSALave_'//CDATE//'.sol'
            OPEN(LU,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
            REWIND(LU)
            CALL PUT_BANNER (NE, NP, LU, 4)
            WRITE(LU) 1.0,NQAL,NP,(BSAVE(K),K=1,NP),NE,(IMAT(K),K=1,NE)
            CLOSE(LU)
            FILENAME='SVELave_'//CDATE//'.sol'
            OPEN(LU,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
            REWIND(LU)
            CALL PUT_BANNER (NE, NP, LU, 2)
            WRITE(LU) 1.0,NP,((SVAVE(J,K),J=1,3),K=1,NP), 
     &            (NDRY(K),K=1,NP),NE,(IMAT(K),K=1,NE),(WSEL(K),K=1,NP)
            CLOSE(LU)
            FILENAME='BVELave_'//CDATE//'.sol'
            OPEN(LU,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
            REWIND(LU)
            CALL PUT_BANNER (NE, NP, LU, 2)
            WRITE(LU) 1.0,NP,((BVAVE(J,K),J=1,3),K=1,NP), 
     &            (NDRY(K),K=1,NP),NE,(IMAT(K),K=1,NE),(WSEL(K),K=1,NP)
            CLOSE(LU)
            COUNT=0.0
            DO I = 1,MAXN
              SSAVE(I) = 0.0
              BSAVE(I) = 0.0
              SVAVE(1,I) = 0.0
              SVAVE(2,I) = 0.0
              SVAVE(3,I) = 0.0
              BVAVE(1,I) = 0.0
              BVAVE(2,I) = 0.0
              BVAVE(3,I) = 0.0
            END DO
          ENDIF
          WRITE(CDATE,'(I2.2,"-",I4.4)') IMON(ITIME),IYR(ITIME)
          CLOSE(LUSVEL)
          FILENAME='SVEL_'//CDATE//'.sol'
          OPEN(LUSVEL,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
          REWIND(LUSVEL)
          CALL PUT_BANNER (NE, NP, LUSVEL, 2)
          CLOSE(LUBVEL)
          FILENAME='BVEL_'//CDATE//'.sol'
          OPEN(LUBVEL,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
          REWIND(LUBVEL)
          CALL PUT_BANNER (NE, NP, LUBVEL, 2)
          CLOSE(LUSSAL)
          FILENAME='SSAL_'//CDATE//'.sol'
          OPEN(LUSSAL,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
          REWIND(LUSSAL)
          CALL PUT_BANNER (NE, NP, LUSSAL, 4)
          CLOSE(LUBSAL)
          FILENAME='BSAL_'//CDATE//'.sol'
          OPEN(LUBSAL,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
          REWIND(LUBSAL)
          CALL PUT_BANNER (NE, NP, LUBSAL, 4)
          IMON_LST=IMON(ITIME)
          IYR_LST=IYR(ITIME)
        ENDIF

        COUNT=COUNT+1.0
        WRITE(LUSVEL) TIME,NP,((XVEL(J,K),J=1,3),K=1,NP), 
     &                (NDRY(K),K=1,NP),NE,(IMAT(K),K=1,NE), 
     &                (WSEL(K),K=1,NP)
        WRITE(LUBVEL) TIME,NP,((XVEL(J,NBOT(K)),J=1,3),K=1,NP), 
     &                (NDRY(K),K=1,NP),NE,(IMAT(K),K=1,NE), 
     &                (WSEL(K),K=1,NP)
        WRITE(LUSSAL) TIME, NQAL, NP, (XVEL(4,K),K=1,NP),
     &                NE,(IMAT(K),K=1,NE)
        WRITE(LUBSAL) TIME, NQAL, NP, (XVEL(4,NBOT(K)),K=1,NP),
     &                NE,(IMAT(K),K=1,NE)

        DO I=1,NP
          SVAVE(1,I)=SVAVE(1,I)+XVEL(1,I)
          SVAVE(2,I)=SVAVE(2,I)+XVEL(2,I)
          SVAVE(3,I)=SVAVE(3,I)+XVEL(3,I)
          SSAVE(I)=SSAVE(I)+XVEL(4,I)
          BVAVE(1,I)=BVAVE(1,I)+XVEL(1,NBOT(I))
          BVAVE(2,I)=BVAVE(2,I)+XVEL(2,NBOT(I))
          BVAVE(3,I)=BVAVE(3,I)+XVEL(3,NBOT(I))
          BSAVE(I)=BSAVE(I)+XVEL(4,NBOT(I))
        ENDDO

        IF(NNODES.GT.0) THEN
           DO I=1,NNODES
             INM=20+I
             WRITE(INM,1000) TIME,XVEL(1,NODEVS(I)),XVEL(2,NODEVS(I)),
     &                XVEL(1,NBOT(NODEVS(I))),XVEL(2,NBOT(NODEVS(I)))

 1000        FORMAT(F9.2,2X,4F12.6)

             INM=50+I
             WRITE(INM,1100) TIME,XVEL(4,NODEVS(I)),
     &                            XVEL(4,NBOT(NODEVS(I)))
 1100        FORMAT(F9.2,2X,2F12.6)
           ENDDO
        ENDIF

      ENDDO

  900 CONTINUE

      CLOSE(LUSVEL)
      CLOSE(LUBVEL)
      CLOSE(LUSSAL)
      CLOSE(LUBSAL)

      IF(NNODES.GT.0) THEN
        DO I=1,NNODES
          INM=20+I
          CLOSE(INM)
          INM=50+I
          CLOSE(INM)
        ENDDO
      ENDIF

      IF(COUNT.GT.0.0) THEN
        DO I=1,NP
          SSAVE(I)=SSAVE(I)/COUNT
          BSAVE(I)=BSAVE(I)/COUNT
          SVAVE(1,I)=SVAVE(1,I)/COUNT
          SVAVE(2,I)=SVAVE(2,I)/COUNT
          SVAVE(3,I)=SVAVE(3,I)/COUNT
          BVAVE(1,I)=BVAVE(1,I)/COUNT
          BVAVE(2,I)=BVAVE(2,I)/COUNT
          BVAVE(3,I)=BVAVE(3,I)/COUNT
        ENDDO
        LU=14
        TIME=1.0
        WRITE(CDATE,'(I2.2,"-",I4.4)') IMON_LST,IYR_LST
        FILENAME='SSALave_'//CDATE//'.sol'
        OPEN(LU,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
        REWIND(LU)
        CALL PUT_BANNER (NE, NP, LU, 4)
        WRITE(LU) TIME,NQAL,NP,(SSAVE(K),K=1,NP),NE,(IMAT(K),K=1,NE)
        CLOSE(LU)
        FILENAME='BSALave_'//CDATE//'.sol'
        OPEN(LU,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
        REWIND(LU)
        CALL PUT_BANNER (NE, NP, LU, 4)
        WRITE(LU) TIME,NQAL,NP,(BSAVE(K),K=1,NP),NE,(IMAT(K),K=1,NE)
        CLOSE(LU)
        FILENAME='SVELave_'//CDATE//'.sol'
        OPEN(LU,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
        REWIND(LU)
        CALL PUT_BANNER (NE, NP, LU, 2)
        WRITE(LU) TIME,NP,((SVAVE(J,K),J=1,3),K=1,NP), 
     &            (NDRY(K),K=1,NP),NE,(IMAT(K),K=1,NE),(WSEL(K),K=1,NP)
        CLOSE(LU)
        FILENAME='BVELave_'//CDATE//'.sol'
        OPEN(LU,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
        REWIND(LU)
        CALL PUT_BANNER (NE, NP, LU, 2)
        WRITE(LU) TIME,NP,((BVAVE(J,K),J=1,3),K=1,NP), 
     &            (NDRY(K),K=1,NP),NE,(IMAT(K),K=1,NE),(WSEL(K),K=1,NP)
        CLOSE(LU)

      ENDIF

      CLOSE(15)

      END
      SUBROUTINE PUT_BANNER (NE, NP, LUOUT, IFT)

      CHARACTER HEADER2*77, HEADER4*77
      DIMENSION IREC(40),FREC(40), IPACKT(1200), IPACKH(77)
      DATA HEADER2 /'  VELOCITIES FROM RMA10 3D FILE  '/
      DATA HEADER4 /'  SALINITIES FROM RMA10 3D FILE  '/
      DATA IREC/40*0/, FREC/40*0.0/, IPACKT/1200*46/

      IF(IFT.EQ.2) THEN
        IFST=120
        IREC(1)=435
        DO I=1,77
          IPACKH(I)=ICHAR(HEADER2(I:I))
        ENDDO
      ELSEIF(IFT.EQ.4) THEN
        IFST=140
        IREC(1)=427
        DO I=1,77
          IPACKH(I)=ICHAR(HEADER4(I:I))
        ENDDO
      ENDIF
      WRITE(LUOUT) IFST, IREC(1), NP, NE
      WRITE(LUOUT) 1200,(IPACKT(ID),ID= 1,1200)
      WRITE(LUOUT) 40,40,(IREC(ID),ID=1,40),(FREC(ID),ID=1,40)
      WRITE(LUOUT) 77,(IPACKH(ID),ID= 1,77)         

      RETURN
      END 
