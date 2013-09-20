      PROGRAM ADPDATA 
     
      real speed(20), dir(20), depth, davg_speed, davg_dir, f_speed_bot,
     &      f_speed_sur, f_speed_davg, f_dir_bot, f_dir_sur, f_dir_savg
      integer choice, cells, meas, year, month, day, hour, min, sec,
     &      j, i
      character filename*90

      write(*,*) 'enter filename of the ADP text data'
      read(*,'(a)') filename
      open(8,file=filename,form='formatted',status='old')
      rewind(8)

100   write(*,*)'What data do you want to save?'
      write(*,*)'1 = cell speeds and directions'
      write(*,*)'2 = depth averaged speeds and directions'
      write(*,*)'3 = filtered speeds and directions'
      read(*,*) choice

      write(*,*) 'enter output file name:'
      read(*,'(a)') filename
      open(10,file=filename,form='formatted',status='unknown')
      rewind(10)
     
      if (choice .EQ. 1) then
        write(10,*)'Cell speeds and directions'
      else if (choice .EQ. 2) then
        write(10,*)'Depth averaged speeds and directions'
      else if (choice .EQ. 3) then
        write(10,*)'Filtered speeds and directions - bottom, surface,
     &depth averaged'
      else
         write(*,*)'Bad choice, try again...'
         go to 100
      end if

      write(10,*)'Measurement#   Year    Month    Day    Hour   Minute
     &Second   Depth    Cells    Speed    Direction'
        

        do i=1,9999999
          read(8,end=300,*) meas, year, month, day, hour, min, sec, 
     &depth, cells
          write(*,*) 'Measurement #.....', meas 
          do j=1,cells
            read(8,end=300,*) speed(j), dir(j)
          end do
          read(8,end=300,*) davg_speed, davg_dir 
          if(cells .EQ. 1) then
            read(8,end=300,*)  f_speed_sur,
     &f_dir_sur, f_speed_davg, f_dir_davg
          else
          read(8,end=300,*) f_speed_bot, f_dir_bot, f_speed_sur,
     &f_dir_sur, f_speed_davg, f_dir_davg
          end if
          if (choice .EQ. 1) then
            write(10,200) meas, year, month, day, hour, min, sec, 
     &depth, cells, ((speed(j), dir(j)),j=1,cells)
          else if (choice .EQ. 2) then
            write(10,210) meas, year, month, day, hour, min, sec, 
     &depth, cells,davg_speed, davg_dir 
          else
            if(cells .EQ. 1) then
              write(10,215) meas, year, month, day, hour, min, sec, 
     &depth, cells, '  ***********    ***********  ', f_speed_sur, 
     &f_dir_sur, f_speed_davg, f_dir_davg 
            else
              write(10,220) meas, year, month, day, hour, min, sec, 
     &depth, cells,f_speed_bot, f_dir_bot, f_speed_sur, f_dir_sur,
     &f_speed_davg, f_dir_davg 
            end if
          end if
           
        end do

  150   format(11A15) 
  200   format(7I15,F15.2,I15,12(F15.3,F15.3)) 
  210   format(7I15,F15.2,I15,F15.3,F15.3) 
  215   format(7I15,F15.2,I15,A,2(F15.3,F15.3)) 
  220   format(7I15,F15.2,I15,3(F15.3,F15.3)) 
  300   continue
        close(8)
        close(10)

      end
