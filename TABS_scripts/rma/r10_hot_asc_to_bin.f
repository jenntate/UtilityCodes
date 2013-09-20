      program r10_hot_asc_to_bin
C
      parameter(mnp=100000,mel=50000,mndf=6,mngs=10,mnblay=10)
C
      dimension iresav(9)
C
      dimension vel(mndf,mnp),vold(mndf,mnp),wsel(mnp),vvel(mnp)
      dimension csusf(mnp,mngs),csusfo(mnp,mngs),dcsot(mnp,mngs)
      dimension dcsoto(mnp,mngs),aomb(mnp),aombo(mnp),aodot(mnp)
      dimension aodoto(mnp),cs(mnp,mnblay,mngs),srco(mnp,mngs)
      dimension hs(mnblay,mnp),hs0(mnblay,mnp)
C
      character*80 binfile, ascfile
C
      write(*,*) '***  r10_hot_asc_to_bin ***'
      write(*,*)
      write(*,*) 'enter the ascii file name'
      read(*,5) ascfile
      write(*,*) 'enter the desired hotstart filename'
      read(*,5) binfile
 5    format(a)
C
      open(10,file=ascfile,form='formatted',status='old')
      open(20,file=binfile,form='unformatted',status='unknown')
C



        READ(10,*)  NDUM,TET,NPX,NDX,NPMX,NBLAX,NGSX,
     *            ((VEL(J,I),VOLD(J,I),J=1,NDX),
     *                          VVEL(I),I=1,NPX)
     *             ,((CSUSF(J,L),CSUSFO(J,L),DCSOT(J,L),DCSOTO(J,L)
     *             ,J=1,NPX),L=1,NGSX)
     *             ,(AOMB(J),AOMBO(J),AODOT(J),AODOTO(J),J=1,NPX)
     *             ,(((CS(J,K,L),J=1,NPMX),K=1,NBLAX),L=1,NGSX)
     *             ,((SRCO(J,L),L=1,NGSX),J=1,NPMX)
     *             ,((HS(J,K),K=1,NBLAX),J=1,NPMX)
     *             ,((HS0(J,K),K=1,NBLAX),J=1,NPMX)
C
      WRITE(20)   NDUM,TET,NPX,NDX,NPMX,NBLAX,NGSX,
     *            ((VEL(J,I),VOLD(J,I),J=1,NDX),
     *                          VVEL(I),I=1,NPX)
     *             ,((CSUSF(J,L),CSUSFO(J,L),DCSOT(J,L),DCSOTO(J,L)
     *             ,J=1,NPX),L=1,NGSX)
     *             ,(AOMB(J),AOMBO(J),AODOT(J),AODOTO(J),J=1,NPX)
     *             ,(((CS(J,K,L),J=1,NPMX),K=1,NBLAX),L=1,NGSX)
     *             ,((SRCO(J,L),L=1,NGSX),J=1,NPMX)
     *             ,((HS(J,K),K=1,NBLAX),J=1,NPMX)
     *             ,((HS0(J,K),K=1,NBLAX),J=1,NPMX)


 100  continue
      write(*,*) 'done'
      stop
      end
