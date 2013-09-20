      PROGRAM VEL_EXCEED
      PARAMETER (MAXNE = 400000, MAXN = 400000, MAXNDF = 10)

      DIMENSION NOP(MAXNE,20), NDEP(MAXN), NREF(MAXN), CORD(MAXN,3),
     &          XVEL(MAXNDF,MAXN), AO(MAXN), DELBED(MAXN), BSHR(MAXN)
      DIMENSION VVEL(MAXN), WSEL(MAXN), NDRY(MAXN),
     &          IMAT(MAXNE), NSURF(MAXN), IRESAV(9)
      DIMENSION NBOT(MAXN)

      CHARACTER filename*80,CHND*6
      REAL      SURF_VELAVG, BOT_VELAVG
      

      NQAL = 1
      DO I = 1,MAXN
        DO J = 1,MAXNDF
          XVEL(J,I) = 0.0
        END DO
        DELBED(I) = 0.0
        BSHR(I) = 0.0
      END DO

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

      WRITE(*,*) 'Enter ascii file for velocity exceedence times'
      READ(*,'(A)') filename
      IF(filename .NE. '') THEN
         OPEN(16,file=filename,form='formatted',status='unknown')
         REWIND(16)
          WRITE(16,2000) 'TIME','NODE','SURF-VEL-X','SURF-VEL-Y',
     &                           'BOTT-VEL-X','BOTT-VEL-Y'
 2000     FORMAT(A9,2X,A9,2X,A12,A12,A12,A12)
      ENDIF

      WRITE(*,*) 'Minimum and maximum hours to be extracted?'
      READ(*,*) TSMIN, TSMAX

      COUNT=0.0
      DO ITIME = 1, 9999999
        READ (15,END=900) TIME, NP3, NDF, NE1,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((XVEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMAT(J), J = 1, NE1), (NDRY(J), J = 1, NP3),
     &       (DELBED(J),J=1,IRESAV(7)),(BSHR(J),J=1,IRESAV(NDFS)),
     &       (VVEL(J), J = 1, NP3), (DUM, J = 1, NE1)

        IF(TIME.GT.TSMAX) GO TO 900
        IF(TIME.GE.TSMIN) THEN

        DO K=1,NP
          SURF_VELAVG = (XVEL(1,K)*XVEL(1,K)+XVEL(2,K)*XVEL(2,K))**0.5
          BOT_VELAVG = (XVEL(1,NBOT(K))*XVEL(1,NBOT(K))+XVEL(2,NBOT(K))
     &                   *XVEL(2,NBOT(K)))**0.5
          IF((SURF_VELAVG .GT. 8.0).OR.(BOT_VELAVG .GT. 8.0)) THEN
            WRITE(*,*) TIME, K, SURF_VELAVG, BOT_VELAVG
            WRITE(16,3000) TIME,K,XVEL(1,K),XVEL(2,K),XVEL(1,NBOT(K)),
     &         XVEL(2,NBOT(K))
 3000     FORMAT(F9.2,2X,I9,2X,4F12.6)
          ENDIF
        ENDDO

        ENDIF

      ENDDO

  900 CONTINUE

      CLOSE(15)
      CLOSE(16)


      END 
