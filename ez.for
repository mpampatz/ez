      program ez
      parameter (maxgridsize=52)
      dimension x(maxgridsize),y(maxgridsize)
      integer gridsize, nowt
      integer c(maxgridsize), c_opt(maxgridsize)
    
c     διαβαζει το αρχειο grid.dat με τις υποψηφιες θεσεις.
c     το grid.dat πρεπει να εχει πρωτη γραμμη 
c     το πληθος των υποψηφιων θεσεων που ακολουθουν και
c     το πληθος των ανεμογεννητριων.
c     στις επομενες γραμμες τις συντεταγμενες των 
c     υποψηφιων θεσεων οπως θα γραφονταν στο task.dat

      open(1,file='grid.dat')
      read (1,*) gridsize, nowt
      do i=1,gridsize
        read(1,*) x(i),y(i)
      enddo
      close(1)
      

c combinations loop

c     πρώτος συνδιασμος
      n=gridsize
      k=nowt

      do i=1,k
        c(i)=i
      enddo

c --ΓΙΑ ΚΑΘΕ ΣΥΝΔΙΑΣΜΟ--:

c   1.φτιαχνει το task.dat

   10 open(2,file='task.dat')
      write(2,*) nowt
      do i=1,k
        write(2,*) x(c(i)), y(c(i))
      enddo
      close(2)

c   2.τρεχει το wake.exe

      call system('start /b /wait wake.exe > nul')

c   3.διαβαζει το results.dat και ελεγχει
c     αν ειναι το βελτιστο που εχει εμφανιστει
c     μεχρι αυτον τον συνδιασμο

      open(3,file='results.dat')
      read(3,*)
      read(3,*) result_new
      if (result_new > result) then
         c_opt=c
         result=result_new
      endif
      close(3)
  
c   4.παραγει τον επομενο συνδιασμο και
c     γυριζει στο "1" με τον τον νεο συνδιασμο 

      do i=k,1,-1
        if ( c(i) < n-k+i ) then
        c(i)=c(i)+1
        do j=i+1,k,1
          c(j)=c(j-1)+1
        enddo
        goto 10
        end if
      enddo 

c   5.οταν τελειωσει τους συνδιασμους
c     μας δινει βελτιστο results.dat για βελτιστο συνδιασμο

      write(*,*) result
      do i=1,k
      write(*,*) c_opt(i) , x(c_opt(i)), y(c_opt(i))
      enddo
      
      end
