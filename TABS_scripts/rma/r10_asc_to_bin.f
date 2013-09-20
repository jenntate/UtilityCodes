      program r10_asc_to_bin
C
      parameter(mnp=100000,mel=50000,mndf=6)
C
      dimension iresav(9)
C
      dimension vel(mndf,mnp),wsel(mnp),vvel(mnp),dfct(mel)
      dimension delbed(mnp),bshr(mnp)
      dimension imatw(mel),ndry(mnp)
C
      character*80 binfile, ascfile
C
      write(*,*) '***  r10_asc_to_bin ***'
      write(*,*)
      write(*,*) 'enter the r10 ascii solution file name'
      read(*,5) ascfile
      write(*,*) 'enter the desired binary filename'
      read(*,5) binfile
 5    format(a)
C
      open(10,file=binfile,form='unformatted',status='unknown')
      open(20,file=ascfile,form='formatted',status='old')
C



      NDFP1 = 7
      do i = 1, 9999
        READ(20,*,END=100) TET, NP, NDF, NE,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((VEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMATW(J), J = 1, NE), (NDRY(J), J = 1, NP),
     &       (DELBED(J),J=1,IRESAV(NDFP1)),(BSHR(J),J=1,IRESAV(NDFS)),
     &       (VVEL(J), J = 1, NP), (DFCT(J),
     &       J = 1, NE)
C
        WRITE(10) TET, NP, NDF, NE,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((VEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMATW(J), J = 1, NE), (NDRY(J), J = 1, NP),
     &       (DELBED(J),J=1,IRESAV(NDFP1)),(BSHR(J),J=1,IRESAV(NDFS)),
     &       (VVEL(J), J = 1, NP), (DFCT(J),
     &       J = 1, NE)
      end do
 100  continue
      write(*,*) 'done'
      stop
      end
