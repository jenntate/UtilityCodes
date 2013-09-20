      program compute_mc_time_ser
      character*80 fname
C
      write(*,*) 'Computes mass conservative series'
      write(*,*) 'by integrating a time series with very'
      write(*,*) 'frequent output and writing out a'
      write(*,*) 'time series at some desired less '
      write(*,*) 'frequent time step'
C
      write(*,*) 'Enter the imput time series file name'
      write(*,*) 'it should have no headers'
      write(*,*) 'just 2 columns: time and value'
C
      write(*,*)
      read(*,*) fname
      open(10,file=fname,form='formatted',status='old')
C
      write(*,*)
      write(*,*) 'Enter the output file name'
      read(*,*) fname
      open(20,file=fname,form='formatted',status='unknown')
C
      write(*,*)
      write(*,*) 'enter the frequency of output'
      write(*,*) 'i.e. the time step'
C
      read(*,*) tstep
C
      write(*,*) 'Now make a wish...'
      write(*,*)
C
      timcum = 0.0
      valint = 0.0
      timcumo = 0.0
      vchkin = 0.0
      vchkout = 0.0
      tcout = 0.0
      timeo = 0.0
      do i = 1, 1000000
       read(10,*,end=100) time, value
       tst = time - timeo
       if (i .eq. 1) tcout = time
       if (i .eq. 1) tst = 0.0
       vchkin = vchkin + value * tst
       timcum = timcum + tst
       if (timcum .ge. tstep) then
         valint = valint + value * (tstep - timcumo)
         valout = valint / tstep
         vchkout = vchkout + valout * tstep
         tcout = tcout + tstep
         write(20,1000) tcout, valout
         valint = value * (timcum - tstep)
         timcum = timcum - tstep
       else
         valint = valint + value * (timcum - timcumo)
       end if
       timcumo = timcum
       timeo = time
      end do

 100  continue
      write(*,*) 'mass balance check'
      write(*,2000) vchkin
      write(*,2100) vchkout
 1000 format(2f15.3)
 2000 format('mass in = ', f18.3)
 2100 format('mass out = ', f18.3)
      stop
      end
