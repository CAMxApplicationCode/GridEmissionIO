      program Grid_emis_modify
c
c
c Program:	ptsr_change  
c
c Language:	Fortran90
c Programmer:	Chitsan Wang 
c Version:	0.0.5
	  parameter(MXY=300, MXZ=16, MXSPEC=200, MXPT=700000)
c 177337 is the x location
	  character*20 a, b
	  character*35 f
	  character*60 inputname
	  character*60 outputname
      character*10 ftype
      character*4 fname(10), spname(10,MXSPEC)
      character*4 note(60)
      integer iloc(4,MXY), LC, cn, pt(3),c,d,p,q,nrawp,ngroupsp,V,W
      integer idumx(MXPT), idumy(MXPT)
      integer icell(MXPT), jcell(MXPT), kcell(MXPT)
      real    temp(MXY,MXY), bconc(MXZ,MXY), locate(2000,20)
      real    emiss(MXPT), flow(MXPT), plumht(MXPT),emissm(MXPT)
      real    xc(MXPT),yc(MXPT),sh(MXPT),X,Y,ER,E
      real    sd(MXPT),st(MXPT),sv(MXPT)
      logical lhdrspec, l3d, lbndry, lptsrc
c read and setup basic parameters    
	  a="./emis_mol.low_noOG."
	  b="./emis_mol.low_wtMA."
	  f=".12km.3SAQS_base08b.cb5p25-soa.camx"
	  write(*,*) 'input data'
c	  read (*,*) inputname
	  write(*,*) 'output data'
c	  read (*,*) outputname  
c	  write(*,*)a
c	  write(*,*)b
	  write(*,*) 'input spec sequence'
	  read (*,*) Spec  
c	  write(*,*) 'input the times of increasing emission rate'
c	  read (*,*) ER
	  CALL csvread(locate,nrawp,ngroupsp)
c-----------------------------------------------------------
c	  do 111 j=1,31
	  write(inputname,'(A20,I7,A35)')a,2008172,f
	  write(*,*)inputname
	  write(outputname,'(A20,I7,A35)')b,2008172,f
	  write(*,*)outputname
	  open(10,file=inputname, form='unformatted',status='old')
	  open(11,file=outputname, form='unformatted',status='new')
c     
		read (10) fname,note,nseg,nspec,idate,begtim,jdate,endtim
		write(11) fname,note,nseg,nspec,idate,begtim,jdate,endtim
c---
		read (10) orgx,orgy,iutm,utmx,utmy,dx,dy,nx,ny,nz,nzlo,nzup,hts,htl,htu
		write(11) orgx,orgy,iutm,utmx,utmy,dx,dy,nx,ny,nz,nzlo,nzup,hts,htl,htu
c---
		read (10) i1,j1,nx1,ny1
		write(11) i1,j1,nx1,ny1
c---      
		read (10) ((spname(m,l),m=1,10),l=1,nspec)
		write(*,903) ((spname(m,l),m=1,10),l=1,nspec)
		write(11) ((spname(m,l),m=1,10),l=1,nspec)
c---
c		read (10) iseg,npmax
c		write(11) iseg,npmax
c---
c		read (10) ((xc(ip),yc(ip),sh(ip),sd(ip),st(ip),sv(ip)),ip=1,npmax)
c		write(11) (xc(ip),yc(ip),sh(ip),sd(ip),st(ip),sv(ip),ip=1,npmax)
c-----------------------------------------------------------------------------------
c   case uamiv
        do 100 ihr = 1,999
        read (10,end=999) ibgdat,begtim,iendat,endtim
        write(*,904) ibgdat,begtim,iendat,endtim
        write(11) ibgdat,begtim,iendat,endtim
		write(*,*) "wang5"
          do 110 l = 1,nspec
		  write(*,*) "wang4"
            do 120 k = 1,1
c			  write(*,*) "wang3"
              read (10) idum, (spname(m,l),m = 1,10), 
     &                        ((temp(i,j),i=1,nx),j=1,ny)
			  if (l .EQ. Spec) then
			  write(*,905) idum, (spname(m,l),m=1,10)
			    do p=1,nrawp
c				  write(*,*)(locate(p,q),q=3,4)
				  V=locate(p,3)
				  W=locate(p,4)
				  E=locate(p,5)
				  write(*,*)'before',V,W,temp(V,W)
				  temp(V,W)=temp(V,W)+E
				  write(*,*)'after',V,W,temp(V,W)
c				  write(*,*) "wang1"
			    enddo
c			  write(*,*) "wang2"
			  endif
              write(11) idum, (spname(m,l),m = 1,10), 
     &                        ((temp(i,j),i=1,nx),j=1,ny)
 120        continue    
 110      continue 
 100    continue
c    case ptsr
c 100		read (10,end=800) ibgdat,begtim,iendat,endtim
c			write(*,904) ibgdat,begtim,iendat,endtim
c			write(11) ibgdat,begtim,iendat,endtim
c			read (10) iseg,numpts
c			write(11) iseg,numpts
c			read (10) (icell(ip),jcell(ip),kcell(ip),flow(ip),
c    &                     plumht(ip),ip=1,numpts)
c			write(11) (icell(ip),jcell(ip),kcell(ip),flow(ip),
c    &                     plumht(ip),ip=1,numpts)
c			do 110 l = 1,nspec
c				read (10) iseg, (spname(m,l),m=1,10),((temp(i,j),i=1,nx),j=1,ny)
c				if (l .EQ. Spec) then
c					write(*,903) iseg, (spname(m,l),m=1,10)
c           do 120 k = 1,nlay
c              read (10,905,err=800) idum, (spname(m,l),m=1,10)
c              read (10,906,err=800) ((temp(i,j),i=1,nx),j=1,ny)
c              write(11) idum, (spname(m,l),m = 1,10), 
c     &                  ((temp(i,j),i=1,nx),j=1,ny)
c 120        continue    
c 110      continue    
 
c			enddo
c---  end loop over hours
	  close(10)
      close(11)
 800  write(*,*) "End of File"
c111  continue
c
c---  I/O error messages ---
c
 900  format(10a1,60a1,/,i2,1x,i2,1x,i6,f6.0,i6,f6.0)
 901  format(2(f16.5,1x),i3,1x,4(f16.5,1x),5i4,3f7.0)
 902  format(4i5) 
 903  format(10a1)
 904  format(5x,2(i10,f10.2))
 905  format(i4,10a1)
 906  format(5e14.7)
 908  format(3i10/(5i14))
 909  format(i10,10a1,i10/(5e14.7))
 910  format(2i10)
 911  format(2(f16.5,1x),4e14.7)
 912  format(3i12,2e14.7)
 913  format(5e14.7)
 914  format(1a40,1I5)
 915  format(1a45,1I5)
c
 999  stop
      end
