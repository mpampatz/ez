      program ez
      parameter (maxgridsize=52)
      dimension x(maxgridsize),y(maxgridsize)
      integer gridsize, nowt
      integer c(maxgridsize), c_opt(maxgridsize)
    
c     �������� �� ������ grid.dat �� ��� ��������� ������.
c     �� grid.dat ������ �� ���� ����� ������ 
c     �� ������ ��� ��������� ������ ��� ���������� ���
c     �� ������ ��� ���������������.
c     ���� �������� ������� ��� ������������� ��� 
c     ��������� ������ ���� �� ��������� ��� task.dat

      open(1,file='grid.dat')
      read (1,*) gridsize, nowt
      do i=1,gridsize
        read(1,*) x(i),y(i)
      enddo
      close(1)
      

c combinations loop

c     ������ ����������
      n=gridsize
      k=nowt

      do i=1,k
        c(i)=i
      enddo

c --��� ���� ���������--:

c   1.�������� �� task.dat

   10 open(2,file='task.dat')
      write(2,*) nowt
      do i=1,k
        write(2,*) x(c(i)), y(c(i))
      enddo
      close(2)

c   2.������ �� wake.exe

      call system('start /b /wait wake.exe > nul')

c   3.�������� �� results.dat ��� �������
c     �� ����� �� �������� ��� ���� ����������
c     ����� ����� ��� ���������

      open(3,file='results.dat')
      read(3,*)
      read(3,*) result_new
      if (result_new > result) then
         c_opt=c
         result=result_new
      endif
      close(3)
  
c   4.������� ��� ������� ��������� ���
c     ������� ��� "1" �� ��� ��� ��� ��������� 

      do i=k,1,-1
        if ( c(i) < n-k+i ) then
        c(i)=c(i)+1
        do j=i+1,k,1
          c(j)=c(j-1)+1
        enddo
        goto 10
        end if
      enddo 

c   5.���� ��������� ���� �����������
c     ��� ����� �������� results.dat ��� �������� ���������

      write(*,*) result
      do i=1,k
      write(*,*) c_opt(i) , x(c_opt(i)), y(c_opt(i))
      enddo
      
      end
