c hymain.for    []
      program hypoel
c hypoellipse -- john c. lahr
c non-unix version of main routine 
c
c************************* notes for programmers **************************
c
c     a binary search of the station list is used in the function phaind.
c         if the search does not work on your computer, use the version
c         of phaind that is commented out, which does a complete search.
c
c     subroutines init, opfls, trvcon, trvdrv, and uamag must have 
c     $notruncate statements added for pc systems so that variable names 
c     longer than 6 characters may be used.
c     
c     all non-unix systems use hymain.for and init.for.  the equivalent
c         routines for unix systems are hypoe.c, setup_server.c, 
c         listen_serv.c, fdgetstr.c, cleanup.f, initial.f, and
c         getbin.f.  
c
c     subroutines openfl and opfls, which open files, have differences between
c         various systems. 
c
c     subroutine openfl also has differences between the unix-masscomp and
c         the unix-sun systems.
c
c     subroutines phagt and npunch use a back slash character, which 
c         must be doubled on unix systems.  
c
c     subroutines dubl, erset, jdate, and timit use non-standard fortran 
c         and must be modified for use with vax, pc, or unix computers.
c
c           dubl     - sets a double precision number equal to a 
c                        single precision number
c           erset    - resets error limits on vax/vms computers
c           jdate    - gets current date and time from the operating system
c           timit    - times the execution on vax/vms computers
c
c     alternat versions of the code is enclosed by 'c* unix', 'c* pc', 
c         or 'c* vax' comment statements in each of the above subroutines.
c

c get filenames, open files and write greeting
      intype = 1
      call init

c read station list, crustal model, and control records
      call input1

c initialize summary of residuals
      call lissum(1)

c read arrival times and locate earthquakes
      call locate(-1)
      stop
      end
c end hymain
c adddly.for  []
      subroutine adddly(nmodel)
c read in delays for model number greater than 5
      include 'params.inc'
      parameter (ndly = 11)
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /ilmpu/ ns
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /logfil/ logfil
      character*4 stard, dnstrg, rshft
      if(ns .eq. 0) then
        write(punt, 18)
        write(logfil, 62)
18      format('xxxerrorxxx delay models may not preceed the ',
     *  'station list')
        stop 'abort from adddly'
      endif
20    read(inpt, '(a)', end=60) icard
      stard = dnstrg(rshft(icard(1:4)))
      if(stard .eq. ' end') return
c get station number (i) for station stard
c     skip station if not on station list
c     ho modificato qui: ho tolto phaind e messo ceck_staz
      call ceck_staz(stard, nsta, ns, i, ierr_ceck)
      if(ierr_ceck .ne. 0) then
        write(punt, 34) stard, icard
        write(logfil, 34) stard, icard
34      format(' ***>', a4, ' is not on station list, so these delays ',
     *  ' will not be used:', /, a)
        goto 20
      endif
c because standard fortran 77 to can not read an internal file with
c free format, file 14 must be used in the following code!
      rewind 14
      write(14, '(a)') icard(5:110)
      rewind 14
c     read(icard(5:110), *) dly(nmodel, i), sdly(nmodel, i)
      read(14, *) dly(nmodel, i), sdly(nmodel, i)
      goto 20
60    write(punt, 62)
      write(logfil, 62)
62    format('xxxerrorxxx end of file found while reading additional',
     * ' delays, so stop')
      stop 'abort from adddly'
      end
c end adddly
c adderr.for    [unix]
      subroutine adderr(zup, zdn)
c adds zup and zdn to the summary record and adds the event
c to file 4, and file 11
      character*117 sumrec
      character*117 arcrec
      character*6 fmit
      common /an/ n14, n15
c* (unix
      call flush (14)
      call flush (15)
c* unix)
c* (pc
c* pc)
c* (vax
c* vax)
      rewind 14
      rewind 15
      if(n14 .gt. 0) then
        if(n14 .gt. 1) then
          print *, ' n14 = ', n14
          print *, ' logic error: n14 should be 0 or 1'
        endif
        do 20 i = 1, n14
          read(14, '(a)') sumrec
          call formal(zup, izup, 2, 0, fmit, azup)
          if (fmit .eq. ' ') then
c           write(sumrec(101:102), '(i2)') izup
            write(sumrec(103:104), '(i2)') izup
          else
c           write(sumrec(101:102), fmit) azup
            write(sumrec(103:104), fmit) azup
          endif
          call formal(zdn, izdn, 2, 0, fmit, azdn)
          if (fmit .eq. ' ') then
c           write(sumrec(103:104), '(i2)') izdn
            write(sumrec(105:106), '(i2)') izdn
          else
c           write(sumrec(103:104), fmit) azdn
            write(sumrec(105:106), fmit) azdn
          endif
          write(4, '(a)') sumrec(1:lentru(sumrec))
20      continue
      endif
      if(n15 .gt. 1) then
        do 30 i = 1, n15
          read(15, '(a)') arcrec
c         if(arcrec(81:81) .ne. '/') then
          if(arcrec(83:83) .ne. '/') then
            write(11, '(a)') arcrec(1:lentru(arcrec))
          else
c summary record
            sumrec = arcrec
            call formal(zup, izup, 2, 0, fmit, azup)
            if (fmit .eq. ' ') then
c             write(sumrec(101:102), '(i2)') izup
              write(sumrec(103:105), '(i2)') izup
            else
c             write(sumrec(101:102), fmit) azup
              write(sumrec(103:105), fmit) azup
            endif
            call formal(zdn, izdn, 2, 0, fmit, azdn)
            if (fmit .eq. ' ') then
c             write(sumrec(103:104), '(i2)') izdn
              write(sumrec(105:106), '(i2)') izdn
            else
c             write(sumrec(103:104), fmit) azdn
              write(sumrec(105:106), fmit) azdn
            endif
c	    print *, 'in adderr, about to write summary rec to unit 11'
c	    print *, 'sumrec = ', sumrec
c	    print *, 'lentru = ', lentru(sumrec)
            write(11, '(a)') sumrec(1:lentru(sumrec))
          endif
30      continue
      endif
c* (unix
      call flush(4)
      call flush(11)
c* unix)
c* (pc
c* pc)
c* (vax
c* vax)
      return
      end
c end adderr
c azwtos.for   []
      subroutine azwtos
c azimuthal weighting of stations by quadrants
      include 'params.inc'
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /amo/ tempg(npa), ntgap
      common /gmost/ az(npa)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /qmost/ wt(npa),z
      dimension tx(4),txn(4),ktx(4),kemp(npa),key(npa)
c count and sort by azimuth stations with weight .ne. 0
      j=0
      do 10 i=1,nr
      if (wt(i) .eq. 0.) goto 10
      j=j+1
      tempg(j)=az(i)
   10 continue
c divide into 4 zones with one axis bisecting largest gap between stations
      call sort(tempg,key,j)
      gap=tempg(1)+360.-tempg(j)
      ig=1
      do 20 i=2,j
      dtemp=tempg(i)-tempg(i-1)
      if (dtemp .le. gap) goto 20
      gap=dtemp
      ig=i
   20 continue
      tx(1)=tempg(ig)-0.5*gap
      tx(2)=tx(1)+90.
      tx(3)=tx(1)+180.
      tx(4)=tx(1)+270.
      do  30 i=1,4
      txn(i)=0.
      if (tx(i) .lt. 0.) tx(i)=tx(i)+360.
      if (tx(i).gt.360.) tx(i)=tx(i)-360.
   30 continue
      call sort(tx,ktx,4)
      do  80 i=1,nr
      if (wt(i) .eq. 0.) goto  80
      if (az(i) .gt. tx(1)) goto  50
   40 txn(1)=txn(1)+1.
      kemp(i)=1
      goto  80
   50 if (az(i) .gt. tx(2)) goto  60
      txn(2)=txn(2)+1.
      kemp(i)=2
      goto  80
   60 if (az(i) .gt. tx(3)) goto  70
      txn(3)=txn(3)+1.
      kemp(i)=3
      goto  80
   70 if (az(i) .gt. tx(4)) goto  40
      txn(4)=txn(4)+1.
      kemp(i) = 4
   80 continue
      xn=4
      if (txn(1).eq.0.) xn=xn-1
      if (txn(2).eq.0.) xn=xn-1
      if (txn(3).eq.0.) xn=xn-1
      if (txn(4).eq.0.) xn=xn-1
      fj=j/xn
      do  90 i=1,nr
      if (wt(i) .eq. 0.) goto  90
      ki=kemp(i)
c new weight equals old weight times total number of stations
c divided by number of zones contatning stations and divided
c------- again by number of zones containing this station
      wt(i)=wt(i)*fj/txn(ki)
   90 continue
      return
      end
c end azwtos
c back.for   []
      subroutine back (delat, delon, newlat, newlon, slat, slon)
c
c-------- back - calculate geocentric coordinates of secondary point from
c            step in latitude (km) and longitude(km)
c
c input:  delat     change in earthquake latitude in km (northward positive)
c         delon     change in earthquake longitude in km (westward positive)
c output: newlat    new earthquake geocentric latitude in radians
c         newlon    new earthquake longitude in radians
c
      real newlat,newlon
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
c
      st0 = cos(slat)
      ct0 = sin(slat)
      call cvrtop(delat,delon,delta,az1)
      if (az1 .lt. 0.0) az1 = az1 + twopi
c use approximation of local radius for derivative of surface
c     distance with geocentric latitude.
c  more accurate formulation would be:
c      drdth = -r**3 * cos(alat)*sin(alat)*( (1.-flat)**(-2) - 1. )/a**2
c      dsd = sqrt(drdth**2 + r**2)
      radius = (cos(slat)**2/equrad**2 + sin(slat)**2/polrad**2)**(-.5)
      sdelt = sin(delta/radius)
      cdelt = cos(delta/radius)
      cz0 = cos(az1)
      ct1 = st0*sdelt*cz0+ct0*cdelt
      call cvrtop(st0*cdelt-ct0*sdelt*cz0, sdelt*sin(az1), st1, dlon)
      newlat = atan2(ct1, st1)
      newlon = slon + dlon
      if (abs(newlon) .gt. pi) newlon = newlon - sign(twopi, newlon)
      return
      end
c end back
c block.for   []
      block data
c initialize constants in common statements
      include 'params.inc'
      parameter (ndly = 11)
      logical good, eoff, supout
      character*4 iahead*60, msta*5, nsta*5, icard*110, uacal*50
      common /char/ iahead, msta(npa), nsta(nsn), icard
      character*1 iclass
      common /dbio/ iclass(0:4)
      common /dbiln/ ioldq
      common /dhio/ nedit,good
      common /dio/ rmsmx,presmx,sresmx,noutmx,nimx,iprun,semx
      common /dph/ noswt, eoff
      common /dhin/ iglob, zup, zdn
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /dhil/ iq,ilat,kms,icat
      character*1 bksrc
      common /dipu/ ipkdly, igsum, bksrc
      common /dit/ tid(lmax,lmmax),did(lmax,lmmax),lowv,modin(mmax+3)
      logical medmag
      common /dinx/ imag,imagsv,medmag
      common /dix/ iexcal, uacal
      common /dmost/ ipun,ivlr,blank
      common /imost/ test(100)
      character*1 iqcls
      common /il1/ iqcls
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /logfil/ logfil
      common /idno/ ksel,ksort
      common /idt/ v(lmax2)
      common /iox/ prr(nsn),iuses
      common /it/ vpvs(lmax2),vpvsm(mmax+3),nttab
      common /ix/ ir,qspa(9,40)
      common /ohq/ gap, supout
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /reloc/ irelo, nreloc
      common /rioq/ been,damp,dmpinc,igo
      character*1 mgndx
      common /xfmno1/ mgndx
      common /ip1/ rsew(4)
      common /ilmpu/ ns
      data uacal/'pub1:[alaska.data]uofacal.dat'/, iexcal/0/
c     data rsew/1., 3., 7.5, 15./, inpt/8/, inmain/8/, injump/12/
c revised default relative weights 4/22/89
      data rsew/1., 5., 10., 20./, inpt/8/, inmain/8/, injump/12/
      data logfil/6/,isa/0/,iqcls/'b'/,damp/0.0010/,bksrc/' '/
      data iprun/1/, supout/.false./, ns/0/
      data iclass/'0','a','b','c','d'/, nttab/0/
      data iahead/'    '/,mgndx/' '/,good/.true./
      data blank/1.e20/
      data test/100*1.23456/, v/lmax2*0.001/, model/npa*1/
      data ivlr,kms,iprn,ipun,imag,iq,ksort,ksel,icat/0,1,1,0,0,2,0,1,1/
      data ioldq/0/,ir/0/,lowv/1/,imagsv/0/,iuses/0/,ipkdly/1/
cds      data tid,did/narray*0.0/, modin/13*0/, iglob/1/
      data tid,did/narray*0.0/, modin/mmaxp3*0/, iglob/1/
      data nedit/0/, eoff/.false./, igsum/1/, irelo/0/, nreloc/0/
      data ilis/1/,medmag/.false./
      end
c end block
c boxau.for    []
      subroutine boxau
c compute rms at auxiliary points and estimate quality
      include 'params.inc' 
      parameter (ndly = 11)
      real latep,lonep,latsv,lonsv
      save latsv, lonsv, rmssv, zsv
      save dez, t6, drsv, kdrsv, diag, kdiag, sum, aalz, aala
      save aalo, idirec, idrc
      character*1 iqa, ins, iew
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /bz/ na
      common /bqz/ avrps,avuse
      common /dmost/ ipun,ivlr,blank
      character*1 iclass
      common /dbio/ iclass(0:4)
      common /gmost/ az(npa)
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /obcfn/ ain(npa)
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /phoqn/ inst,knst
      common /qmost/ wt(npa),z
      common /qmost1/ lonep,ni,latep
      common /qgnotx/ delta(npa)
      common /qgo/ org
      common /rob/ v(4,4),noaxp
      common /rbno/ idip(3),iaaz(3),a(3,3)
      common /rfgnoq/ se(4)
      common /tmost/ x(4,npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      common /zqr/ fno
      dimension drsv(14),kdrsv(14),diag(7),kdiag(7)
      dimension sum(5)
      dimension aalz(14),aala(14),aalo(14)
      dimension vec(3)
      character*2 idirec(7), idrc(7)
      data idirec/' n','se','sw','nw','ne',' z',' e'/
      data aala/1.732,1.,1.,1.,1.,0.,0.,0.,0.,-1.,-1.,-1.,-1.,-1.732/,
     *     aalo/0.,1.,-1.,1.,-1.,0.,1.732,-1.732,0.,1.,-1.,1.,-1.,0./,
     *     aalz/0.,-1.,-1.,1.,1.,-1.732,0.,0.,1.732,-1.,-1.,1.,1.,0./
      kgoon = 0
c
c write heading
   17 if( (iprn .gt. 2)  .or. (noaxp .eq. 1) ) write(punt,18)
   18 format(/50h       lat       lon         z     avrps        no ,
     *  7x,3hrms,25x,4hdrms/)
c
c save current values
      rmssv = rms
      latsv = latep
      lonsv = lonep
      zsv = z
      orgsv = org
      instsv = inst
c set inst = -9 so quakes will compute fixed location rms but will
c not call regres.
      inst = -9
      freor = .true.
      t6 = abs(test(6))/1.732
      dez = t6
      do 70 na = 1, 14
        call back(t6*aala(na),t6*aalo(na),savla,savlo,latsv,lonsv)
        savez = zsv +aalz(na)*dez
        if(z .lt. 0.) z = 0.0
        call quakes
c output rms error af auxiliary points
        rmsx = rms
        drms = rmsx - rmssv
        if(iprn .eq. 4) then
c calcute predicted rms at aux points
          tot = 0.0
          vec(1) = aalo(na)/1.732
          vec(2) = aala(na)/1.732
          vec(3) = aalz(na)/1.732
          do 48 i = 1,3
            do 46 j = 1,3
              tot = tot + vec(i)*vec(j)*v(i,j)
   46       continue
   48     continue
          pdrms = sqrt(rmssv**2 + tot*test(6)*test(6)/fno)
          drms = rmsx - pdrms
        endif
        call unfold2(savla,savlo,la,ins,ala,lo,iew,alo)
        drsv(na) = drms
        if( (iprn .ge. 2) .and. (noaxp .ne. 1) ) then
          goto (1,2,3,4,5,6,7,8,9,10,11,4,13,1), na
    2     write(punt,801) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  801     format(4f10.2,i10,f10.2,10x,f5.2)
          goto  50
    3     write(punt,802) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  802     format(4f10.2,i10,f10.2,24x,1h-,8x,f5.2/96x,1h.)
          goto  50
    1     write(punt,803) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  803     format(/4f10.2,i10,f10.2,23x,f5.2/)
          goto  50
    4     write(punt,804) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  804     format(4f10.2,i10,f10.2,13x,f5.2,19x,1h.)
          goto  50
    5     write(punt,805) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  805     format(4f10.2,i10,f10.2,12x,'|',23x,f5.2)
          goto  50
    6     write(punt,806) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  806     format(4f10.2,i10,f10.2,20x,f5.2,10x,'|')
          goto  50
    7     write(punt,807) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  807     format(4f10.2,i10,f10.2,3x,f5.2/)
          write(punt,815) rmssv
  815     format(50x,f10.2,12x,'|',10x,5h 0.00,10x,'|'/95x,'|')
          goto  50
    8     write(punt,808) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  808     format(4f10.2,i10,f10.2,43x,f5.2)
          goto  50
    9     write(punt,809) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  809     format(4f10.2,i10,f10.2,26x,f5.2)
          goto 50
   10     write(punt,810) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  810     format(4f10.2,i10,f10.2,10x,f5.2,23x,'|')
          goto 50
   11     write(punt,811) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  811     format(4f10.2,i10,f10.2,13x,1h.,10x,1h-,8x,f5.2/
     *    74x,1h.,21x,1h.)
          goto 50
   13     write(punt,813) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  813     format(4f10.2,i10,f10.2,27x,1h-,8x,f5.2)
        endif
   50   continue
   70 continue
c ouality output
      i = 0
      ii = 15
      do  80 iii = 1,7
      i = i + 1
      ii = ii - 1
   80 diag(iii) = (drsv(i) + drsv(ii))/2.0
      call sort(diag, kdiag, 7)
      call sort(drsv, kdrsv, 14)
      do  90 i = 1,7
        j = kdiag(i)
        idrc(i) = idirec(j)
   90 continue
      if((noaxp .eq. 0) .and. (iprn .ge. 0)) write(punt,100) idrc, diag
  100 format(/25x,18hquality evaluation//,
     * 34h diagonals in order of strength   ,7(4x,a2)/,
     *  19h ave. of end points,15x,7f6.2//)
      sum(1) = 0.0
      do 110 i=1,14
c do not use upper point in average if limited by surface
        if((z.lt.abs(test(6))*1.732).and.(kdrsv(i).eq. 6)) goto 110
        sum(1) = sum(1) + drsv(i)
  110 continue
      avdrms = sum(1)/14.
      if(z .lt. abs(test(6))*1.732) avdrms = sum(1)/13.
      drmin = drsv(1)
      icmin = kdrsv(1)
      if((z .ge. abs(test(6))*1.732).or.(kdrsv(1) .ne. 6)) goto 115
      drmin = drsv(2)
      icmin = kdrsv(2)
  115 jab = 4
      if((nrwt.ge. 4.).and.(rmssv.le. 0.4).and.(avdrms.ge. 0.5)) jab = 3
      if((nrwt.ge.5.).and.(rmssv.le. 0.4).and.(drmin  .ge. 0.15)) jab=2
      if((nrwt.ge.6.).and.(rmssv.le. 0.2).and.(drmin  .ge. 0.30)) jab=1
      iqa = iclass(jab)
      if((noaxp.eq.0) .and. (iprn .ge. 0))
     * write(punt,120)nrwt,rmssv,drmin,avdrms,iqa
  120 format(10x,50h    number       rms  min drms  ave drms   quality/
     * 10x,i10,3f10.2,9x,a1/)
c
c restore variable values
      latep = latsv
      lonep = lonsv
      z = zsv
      org = orgsv
      inst = instsv
      if (drmin .gt. -.03 .or. test(6) .gt. 0.) then
c restore all values, such as azimuth, angle of incidence, and
c standard error to those of final solution for npunch.
130     iprnsv = iprn
        iprn = -2
        inst = 9
        freor = .false.
        savla = latsv
        savlo = lonsv
        savez = zsv
        savor = orgsv
        call quakes
c       call output(0)
c       call output(1)
        iprn = iprnsv
        inst = instsv
        return
      else
        kgoon = kgoon + 1
c the first time an event reaches here, kgoon will equal 1.
c the 2nd time kgoon will equal 2, so the event will not be rurun again.
        if((inst .ne. 9) .and. (inst .ne. 8)) return
        if(kgoon .ge. 2) then
c do the same as above, starting with statement 130
c         goto 130
          iprnsv = iprn
          iprn = -2
          inst = 9
          freor = .false.
          savla = latsv
          savlo = lonsv
          savez = zsv
          savor = orgsv
          call quakes
c         call output(0)
c         call output(1)
          iprn = iprnsv
          inst = instsv
          return
        endif
        call back(t6*aala(icmin),t6*aalo(icmin),savla,savlo,latsv,lonsv)
        savez = zsv + aalz(icmin)*dez
        if(z .lt. 0.0) z = 0.0
        write(punt,1000)
 1000   format(///' *** run again starting at best nearby point ***',/)
        call quakes
        goto 17
      endif
      end
c end boxau
c cosh.for    []
      function cosh(x)
      cosh = (exp(x) + exp(-x))/2.
      return
      end
c end cosh
c coshi.for    []
      function coshi(x)
      p = x + sqrt(x*x - 1.)
      coshi = alog(p)
      return
      end
c end coshi
c critic.for    []
      subroutine critic(delta,az,w,iuse,nrp,nr,ldx)
c select critical stations
c   1) closest 4 with p-phase readings.
c   2) additional with p- or s-phase readings if they reduce the a gap > 72 deg
c      by 5 deg or more.
c   3) s is used at critical stations.  if there are no s phases selected,
c      then s is used from the closest non-critical station.
      include 'params.inc' 
      logical swtchp, swtchs
      dimension dtemp(npa),key(npa)
      dimension delta(npa),az(npa),w(npa),iuse(npa),ldx(npa)
      integer punt
      common /punt/ punt
      write(punt,20)
20    format(' determining which stations are critical.')
      do 40 j = 1, nr
        iuse(j) = 0
        if (w(j) .eq. 0.) iuse(j) = 1
40    continue
      do 50 j = 1, nrp
        dtemp(j) = delta(j)
50    continue
      call sort(dtemp,key,nrp)
      ns = 0
      nwt = 0
c always use first 4 with p-wt .gt. 0.0
      do 100 i = 1, nrp
        j = key(i)
c check for p
        if (w(j) .gt. 0.0) then
          nwt = nwt + 1
          iuse(j) = 1
c add s only if p is selected
          if (ldx(j) .ne. 0) then
            k = ldx(j)
            if (w(k) .gt. 0.0) then
              iuse(k)  = 1
              ns = ns + 1
            endif
          endif
        endif
        if (nwt .ge. 5) goto 125
100   continue
c fall through if there are 4 or fewer p phases that have wt <> 0
125   if (nwt .eq. 0) then
        sge72 = 400.
        nge72 = 0.
      else
        call sumgap(iuse, az, nr, sge72, nge72, w)
      endif
c try one at a time for stations that reduce gap.
      do 200 i = 1, nrp
        j = key(i)
        k = ldx(j)
        if (iuse(j) .eq. 1) then
c p already in use
          if (k .gt. 0) then
            if (iuse(k) .eq. 1) then
c   s also in use, so skip on to next phase
              goto 200
            endif
          else
c   no s
            goto 200
          endif
        endif
c either p and/or s are not now being used - check weights
        if (w(j) .eq. 0.) then
          if (k .gt. 0) then
c there is an s phase
            if (w(k) .eq. 0.) then
c p and s weights are 0, so skip to next phase
              goto 200
            endif
          endif
        endif
c either p and/or s has non zero weight and is not yet being used
c try adding the p or s to test gap reduction
        swtchp = .false.
        swtchs = .false.
        if ((iuse(j) .eq. 0) .and. (w(j) .gt. 0.)) then
          swtchp = .true.
          iuse(j) = 1
        else
          swtchs = .true.
          iuse(k) = 1
        endif
        call sumgap(iuse, az, nr, sgtr, ngtr, w)
c check if the number of gaps larger than 72 has been increased
        if (ngtr .gt. nge72) then
          nge72 = ngtr
c check if the reduction in sum of gaps > 72 has been reduced by 5 or more deg
        else if (sgtr .le. (sge72 - 5.)) then
          nge72 = ngtr
          sge72 = sgtr
        else
c there was no or not enough improvement
          if(swtchp) iuse(j) = 0
          if(swtchs) iuse(k) = 0
          goto 200
        endif
c this phase reduced the gap, so use it!
        if (swtchp) then
          nwt = nwt + 1
c add s if available
          if (k .ne. 0) then
            if (w(k) .gt. 0.0) then
              iuse(k)  = 1
              ns = ns + 1
            endif
          endif
        endif
        if (swtchs) then
          ns = ns + 1
        endif
200   continue
400   if (ns .gt. 1) goto 500
c use closest s if none found at any critical station
      do 350 i = 1,nrp
        j = key(i)
        if (ldx(j) .eq. 0) goto 350
        k = ldx(j)
        if (w(k) .le. 0.) goto 350
        iuse(k) = 1
        iuse(i) = 1
        goto 500
350   continue
500   return
      end
c end critic
c cvrtop.for    []
      subroutine cvrtop(x, y, r, theta)
c
c-------- bruce julian
c
c-------- cvrtop - convert from rectangular to polar coordinates
c
c (output - may overlay x, y)
c
c-------- standard fortran funct. required:  atan2
c-------- funct. required:  hypot
c
      r = hypot(x, y)
      theta = 0.
      if ((y .ne. 0.) .or. (x .ne. 0.)) theta = atan2(y, x)
      return
      end
c
c end cvrtop
c cyldly.for    []
      subroutine cyldly
     *  (kno, alat, alon, lat1, lon1, x0, y0, z, dly, sdly, iprn,
     *   modset, test8)
      save cyldy, cylrd, cylrd1, cylup, cylup1, cyldn, cyldn1,
     *  xc, yc, ncyl, setcyl
      real lat1, lon1
      character*1 ins, iew
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (mxcyl = 50)
      logical setcyl
      real     cyld
c              cyld		horizontal distance to inner cylinder
      real     cyldin(mxcyl)
c              cyldin(i)    	distance to cylinder center for eq within
c                               inner radius
      integer  cyldy(mxcyl)
c              cyldy(i)		delay assigned to cylinder i
      integer  cylmd(mxcyl)
c              cylmd(i)		velocity model assigned to cylinder i
      real     cylrd(mxcyl)
c              cylrd(i)         inner radius of cylinder number i
      real     cylrd1(mxcyl)
c              cylrd1(i)        outer radius of cylinder number i
      integer  cyldm(mxcyl)
c              cyldm(i)		delay model for transition table entry i
      integer  cyldmin(mxcyl)
c              cyldmin(i)	delay model for inner table entry i
      real     cylup(mxcyl)
c              cylup(i)         upper limit of inner cylinder i
      real     cylup1(mxcyl)
c              cylup1(i)        upper limit of outer cylinder i
      real     cyldn(mxcyl)
c              cyldn(i)         lower limit of inner cylinder i
      real     cyldn1(mxcyl)
c              cyldn1(i)        lower limit of outer cylinder i
      real     cylwt(mxcyl)
c              cylwt(i)         weight derived for transition zone i
      real     dly(ndly,nsn)
c              dly(i, j)         p-delay for model i, station j
      integer  ntrans
c              ntrans		number of entries in transition table
      real     sdly(ndly,nsn)
c              sdly(i, j)        s-delay for model i, station j
      real     xc(mxcyl)
c              xc(i)            x coordinate of center of cylinder i
      real     yc(mxcyl)
c              yc(i)            y coordinate of center of cylinder i
      real     z
c              z		current trial eq depth
      real     zmarg
c              zmarg		vertical distance from event to endcap
c                               (negative for lower endcap)
      data setcyl /.false./
      modset = 0
      if(iprn .ge. 5) then
        print *, ' '
        print *, 'begin sub. cyldly to select delay and velocity'
	print *, 'model(s) based on cylindrical domains.'
      endif
      if (.not. setcyl) then
cd       print *, 'read in cylinder definitions with cylget'
        call cylget
     *  ( mxcyl, cyldy, cylmd, cylrd, cylrd1, cylup, cylup1, cyldn, 
     *    cyldn1, xc, yc, ncyl, lat1, lon1, test8)
        setcyl = .true.
      endif
c compute the x,y coordinates of the current trial epicenter
      call delaz(lat1, lon1, delt, deldeg, azz, alat, alon)
      x0 = delt*sin(azz*rad)
      y0 = delt*cos(azz*rad)
      if(iprn .ge. 5) then
	call unfold2(alat, alon, la, ins, ala, lo, iew, alo)
	print *, 'current epicenter = ', la, ins, ala, lo, iew, alo
        print *, 'location of epicenter wrt reference station is'
        print *, 'azimuth (deg) = ', azz
        print *, 'distance (km) = ', delt
        print *, 'x, y = ', x0, y0
        print *, 'loop through ', ncyl, ' regions. regions must be in '
        print *, 'order of preference in cases of overlap.'
      endif
      ntrans = 0
      ninner = 0
      do 20 i = 1, ncyl
        cyld = sqrt((x0 - xc(i))**2 + (y0 - yc(i))**2)
         if(iprn .ge. 5) then
	   print *, 'x, y of cylinder ', i, ' is ', xc(i), yc(i)
           print *, 'dist to this cylinder ',
     *     'which uses delay model ', cyldy(i), ' is ', cyld
           print *, 'z = ', z, ' cylup,dn(i) = ', cylup(i), cyldn(i)
        endif
	if ((cyld .le. cylrd(i)) .and. (z .ge. cylup(i)) .and.
     *    (z .le. cyldn(i))) then
          if(iprn .ge. 5) 
     *      print *, 'location is within inner cylinder'
	  ninner = ninner + 1
	  cyldin(ninner) = cyld
	  cyldmin(ninner) = i
        else if ((cyld .le. cylrd1(i)) .and. (z .ge. cylup1(1)) .and.
     *    (z .le. cyldn1(i))) then
c
cd         print *, 'location is within transition zone, so compute '
cd         print *, 'weight and add to list'
          ntrans = ntrans + 1
          cyldm(ntrans) = cyldy(i)
cd         print *, 'zmarg is distance into upper or lower cap'
          zmarg = 0.0
          if ((z .lt. cylup(i)) .and. (z .gt. cylup1(i)))
     *    zmarg = cylup(i) - z
          if ((z .gt. cyldn(i)) .and. (z .lt. cyldn1(i)))
     *    zmarg = cyldn(i) - z
cd         print *, 'note that zmarg will be negative for lower ',
cd    *             'cap region'
          if (zmarg .eq. 0.0) then
c
cd           print *, 'adjacent to cylinder '
cd           print *, 'cyld - cylrd(i) ', cyld - cylrd(i)
cd           print *, 'pi ', pi
cd           print *, 'cylrd1(i) - cylrd(i) ', cylrd1(i) - cylrd(i)
            cylwt(ntrans) = 0.5 + 0.5*cos( pi*(cyld - cylrd(i)) /
     *      (cylrd1(i) - cylrd(i)) )
          else if (cyld .le. cylrd(i)) then
c
cd           print *, 'within end caps'
            if (zmarg .gt. 0) then
cd             print *, 'within upper cap'
              cylwt(ntrans) = 0.5 + 0.5*cos( pi*zmarg /
     *        (cylup(i) - cylup1(i)) )
            else
c
cd             print *, 'within lower cap'
              cylwt(ntrans) = 0.5 + 0.5*cos( pi*zmarg /
     *        (cyldn(i) - cyldn1(i)) )
            endif
          else
c
cd           print *, 'within corner zone'
            if (zmarg .gt. 0) then
cd             print *, 'within upper corner zone'
              cylwt(ntrans) = 0.5 + 0.5*cos(pi*sqrt
     *        ( ((cyld - cylrd(i)) /
     *          (cylrd1(i) - cylrd(i)))**2 +
     *          (zmarg /
     *          (cylup(i) - cylup1(i)))**2 ) )
            else
cd             print *, 'within lower corner zone'
              cylwt(ntrans) = 0.5 + 0.5*cos(pi*sqrt
     *        ( ((cyld - cylrd(i)) /
     *          (cylrd1(i) - cylrd(i)))**2 +
     *          (zmarg /
     *          (cyldn(i) - cyldn1(i)))**2 ) )
            endif
          endif
        endif
20    continue

c use the parameters for the cylinder for which the eq is
c closest to the center
      if (ninner .ne. 0) then
	if(ninner .eq. 1) then
          kno = cyldy(cyldmin(ninner))
 	  modset = cylmd(cyldmin(ninner))
          return
        else
	  ntouse = 1
	  do 22 i = 2, ninner
	    if(cyldin(i) .lt. cyldin(ntouse)) then
	      ntouse = i
	    endif
22        continue
          kno = cyldy(cyldmin(ntouse))
 	  modset = cylmd(cyldmin(ntouse))
          return
	endif
      endif
	    
	  
      if (ntrans .eq. 0) then
cd       print *, 'not within any cylinders, so use default model (1)'
        kno = 1
        return
      else
cd       print *, 'weights: ', (cylwt(i), i = 1, ntrans)
cd       print *, 'models:  ', (cyldm(i), i = 1, ntrans)
c reduce list if there are 2 or more entries
        if (ntrans .gt. 1) call cylred(cyldm, cylwt, ntrans, sumwt)
        kno = ndly
c
        if (ntrans .eq. 1) then
          if (cylwt(1) .ge. 1.0) then
            kno = cyldm(1)
            return
          else
c           fill in with default model
            ntrans = 2
            cylwt(2) = 1.0 - cylwt(1)
            cyldm(2) = 1
            if (iprn .ge. 5) then
              print *, 'weights: ', (cylwt(i), i = 1, ntrans)
              print *, 'models:  ', (cyldm(i), i = 1, ntrans)
            endif
          endif
c
        else if (ntrans .eq. 3) then
          cylwt(1) = cylwt(1)/sumwt
          cylwt(2) = cylwt(2)/sumwt
          cylwt(3) = cylwt(3)/sumwt
          sumwt = 1.0
cd         print *, 'weights: ', (cylwt(i), i = 1, ntrans)
cd         print *, 'models:  ', (cyldm(i), i = 1, ntrans)
        else
c
c         ntrans = 2
          if (sumwt .gt. 1.0) then
            cylwt(1) = cylwt(1)/sumwt
            cylwt(2) = cylwt(2)/sumwt
          else
c           fill in with default model
            ntrans = 3
            cylwt(3) = 1. - sumwt
            sumwt = 1.0
            cyldm(3) = 1
cd           print *, 'weights: ', (cylwt(i), i = 1, ntrans)
cd           print *, 'models:  ', (cyldm(i), i = 1, ntrans)
          endif
        endif
      endif
c compute combined delays
      do 30 i = 1, nsn
        dly(ndly,i) = 0.0
        sdly(ndly,i) = 0.0
        do 28 j = 1, ntrans
          dly(ndly,i) =   dly(ndly,i) +  dly(cyldm(j), i)*cylwt(j)
          sdly(ndly,i) = sdly(ndly,i) + sdly(cyldm(j), i)*cylwt(j)
28      continue
30    continue
      return
      end
c end cyldly
c cylget.for    []
      subroutine cylget
     * ( mxcyl, cyldy, cylmd, cylrd, cylrd1, cylup, cylup1, cyldn, 
     *   cyldn1, xc, yc, ncyl, lat1, lon1, test8 )
      real lat1, lon1
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      character*1 ins, iew
      character*110 record
c read in the cylinders defining weight model regions
      integer  cyldy(mxcyl)
c              cyldy(i)		delay assigned to cylinder i
      integer  cylmd(mxcyl)
c              cylmd(i)         velocity model assigned to cylinder i
      real     cylrd(mxcyl)
c              cylrd(i)         inner radius of cylinder number i
      real     cylrd1(mxcyl)
c              cylrd1(i)        outer radius of cylinder number i
      real     cylup(mxcyl)
c              cylup(i)         upper limit of inner cylinder i
      real     cylup1(mxcyl)
c              cylup1(i)        upper limit of outer cylinder i
      real     cyldn(mxcyl)
c              cyldn(i)         lower limit of inner cylinder i
      real     cyldn1(mxcyl)
c              cyldn1(i)        lower limit of outer cylinder i
      real     xc(mxcyl)
c              xc(i)            x coordinate of center of cylinder i
      real     yc(mxcyl)
c              yc(i)            y coordinate of center of cylinder i
      ncyl = 0
cd     print *, 'read cylinder parameters'
      do 20 i = 1, mxcyl
18      read(17, '(a)', end = 90) record
        if(record(1:2) .eq. 'c*') goto 18
c because standard fortran 77 to can not read an internal file with
c free format, file 14 must be used in the following code!
        rewind 14
        write(14, '(a)') record
        rewind 14
c       read(record, *)
        read(14, *)
     *  cyldy(i),
     *  cylmd(i), blat, blon, cylrd(i), cylrd1(i), cylup(i),
     *  cylup1(i), cyldn(i), cyldn1(i)

c       adjust for the depth of the top of the model
        cylup(i) = cylup(i) + test8
        cylup1(i) = cylup1(i) + test8
        cyldn(i) = cyldn(i) + test8
        cyldn1(i) = cyldn1(i) + test8

c       compute the x,y coordinates of the cylinder center
        nlat = abs(blat)
        rlat = (abs(blat) - nlat)*60.
        ins = 'n'
        if (blat .lt. 0) ins = 's'
        nlon = abs(blon)
        rlon = (abs(blon) - nlon)*60.
        iew = 'w'
        if (blon .lt. 0) iew = 'e'
c convert to geocentric coordinates> alat, alon
cd       print *, 'blat, blon = ', blat, blon
cd       print *, 'nlat, ins, rlat, nlon, iew, rlon'
cd       print *, nlat, ins, rlat, nlon, iew, rlon
        call fold2(alat, alon, nlat, ins, rlat, nlon, iew, rlon)
        call delaz(lat1, lon1, delt, deldeg, azz, alat, alon)
cd       print *, 'location of cylinder wrt reference station is'
cd       print *, 'cyl lat lon = ', alat, alon
cd 	 print *, 'ref sta lat lon = ', lat1, lon1
cd       print *, 'azimuth (deg) = ', azz
cd       print *, 'distance (km) = ', delt
        xc(i) = delt*sin(azz*rad)
        yc(i) = delt*cos(azz*rad)
cd       print *, 'converted to x, y = ', xc(i), yc(i)
cd       print *, 'i, cyldy(i), cylmd(i), cylrd(i), cylrd1(i),', 
cd    *  'cylup(i), cylup1(i), cyldn(i), cyldn1(i)'
cd       print *,  i, cyldy(i), cylmd(i), cylrd(i), cylrd1(i), 
cd    *  cylup(i), cylup1(i), cyldn(i), cyldn1(i)
        if (cyldy(i) .gt. ndly) then
          print *, 'sub. cylget attempting to assign delay model',
     *    cyldy(i)
          print *, 'but only ', ndly, ' are allowed.'
          stop
        endif
20    continue
      ncyl = mxcyl
      return
90    ncyl = i - 1
      close (17)
      return
      end
c end cylget
c cylred.for    []
      subroutine cylred(cyldm, cylwt, ntrans, sumwt)
c reduce delay model list, eliminating duplicates and sorting by weight
      integer cyldm(ntrans)
c             cyldm(i)            delay model for # i
      real    cylwt(ntrans)
c             cylwt(i)            weight for # i
      integer key(50)
c             key                 key to original order of weights
      real    sumwt
c             sumwt               sum of weights of up to first 3
      integer tmpdm(50)
c             tmpdm               temp array of delay models
      real    tmpwt(50)
c             tmpwt               temp array of weights
      ntr = 0
      do 20 i = 1, ntrans
        if(cylwt(i) .gt. 0.0) then
          ntr = ntr + 1
          if(i .lt. ntrans) then
            do 18 j = i+1, ntrans
              if((cyldm(j) .eq. cyldm(i)) .and. (cylwt(j) .ne. 0.)) then
                cylwt(i) = cylwt(i) + cylwt(j)
                cylwt(j) = 0.0
              endif
18          continue
          endif
        endif
20    continue
      ntrans = ntr
c sort the weights
      call sort(cylwt, key, ntrans)
      do 30 i = 1, ntrans
        tmpdm(i) = cyldm(i)
30    continue
      do 40 i = 1, ntrans
        cyldm(ntrans + 1 - i) = tmpdm(key(i))
40    continue
c save weights in temporary arrays
      do 50 i = 1, ntrans
        tmpwt(i) = cylwt(i)
50    continue
c change to ascending order
      if (ntrans .gt. 3) ntrans = 3
      sumwt = 0.0
      do 60 i = 1, ntrans
        cylwt(i) = tmpwt(ntrans + 1 - i)
        sumwt = sumwt + cylwt(i)
60    continue
      return
      end
c end cylred
c delaz.for    []
      subroutine delaz(eqlat, eqlon, dekm, dedeg, az0, slat, slon)
c
c-------- delaz - calculate the distance in km (approx equal to geocentric
c      distance times local radius), and azimuths in radians
c
c input:  slat     station geocentric latitude in radians
c         slon     station longitude in radians
c         eqlat    earthquake geocentric latitude in radians
c         eqlon    earthquake longitude in radians
c output: dekm     distance from earthquake to station in kilometers
c         dedeg    distance from earthqauke to station in degrees
c         az0      azimuth from earthquake to station measured clockwise
c                     from north in degrees
c
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
c
      st0 = cos(eqlat)
      ct0 = sin(eqlat)
c use approximation of local radius for derivative of surface
c     distance with geocentric latitude.
c  more accurate formulation would be:
c      drdth = -r**3 * cos(slat)*sin(slat)*( (1.-flat)**(-2) - 1. )/a**2
c      dsd = sqrt(drdth**2 + r**2)
      radius = (cos(eqlat)**2/equrad**2 +
     *sin(eqlat)**2/polrad**2)**(-.5)
      ct1 = sin(slat)
      st1 = cos(slat)
      sdlon = sin(eqlon-slon)
      cdlon = cos(eqlon-slon)
      cdelt = st0*st1*cdlon+ct0*ct1
      call cvrtop(st0*ct1-st1*ct0*cdlon, st1*sdlon, sdelt, az0)
      dedeg = atan2(sdelt, cdelt)*deg
      dekm = radius*atan2(sdelt, cdelt)
      if (az0 .lt. 0.0) az0 = az0 + twopi
      if (az0 .ge. twopi) az0 = az0 - twopi
      az0 = az0*deg
c calculation of back azimuth if needed
c      call cvrtop(st1*ct0 - st0*ct1*cdlon, (-sdlon)*st0, sdelt, az1)
c      if (az1 .lt. 0.0) az1 = az1 + twopi
      return
      end
c end delaz
c diftim.for    []
      subroutine diftim(icent1, iymd1,ihr1,icymd2,ihr2,iporm,difhr)
c calculate first time minus second time
c iporm  -1  first cnyrmody is smaller than second, set difhr = -1.0
c             0  first cnyrmody is equal to second, calculate difhr
c            +1  first cnyrmody is larger, set difhr = +1.0
c difhr       this is the time difference in hours, if dates are =
c
      if (icent1*1000000+iymd1 .lt. icymd2) goto 100
      if (icent1*1000000+iymd1 .gt. icymd2) goto 200
c equal dates
      iporm = 0
      difhr = ihr1 - ihr2
      return
c smaller first date
  100 iporm = -1
      difhr = -1.
      return
c larger first date
  200 iporm = +1
      difhr = +1.
      return
      end
c end diftim
c dnstrg.for    []
      character*(*) function dnstrg(array)
c
c  program to change string to uppercase
c
c  array - a character variable
      character*(*) array
      integer offset
      data offset/32/
c
c  get length of array
c
      dnstrg = ' '
      lenstr = len(array)
      if (lenstr .eq. 0) return
      do 10 i = 1, lenstr
        ic = ichar(array(i:i))
        if ((ic .ge. 65) .and. (ic .le. 90)) then
          dnstrg(i:i) = char(ic + offset)
        else
          dnstrg(i:i) = array(i:i)
        endif
   10 continue
      return
      end
c end dnstrg
c dubl.for    [unix]
      double precision function dubl(x)
c     the wadati sub uses p and s arrival times that are entered
c     to the nearest .01 sec in single precision.  the purpose of this
c     sub is to convert these numbers to the nearest double
c     precision number, setting less significant decmial digits to zero.
c     this version is 16 times faster than dubl1, which uses internal
c     write and read statements.
c     lahr & stephens   feb 1986
c
c* (vax
c      double precision dfact
c      if (x .eq. 0.) then
c        dubl = 0.d0
c        return
c      endif
c      al = alog10(abs(x))
c      if (al .lt. 0.) al = al - .9999995
c      i = al
c      dfact = 10.d0**(7-i)
c      ix = x*dfact + sign(.5, x)
c      dubl = dflotj(ix)/dfact
c* vax)
c* (pc
c* (unix
      dubl = x
c* unix)
c* pc)
      return
      end
c end dubl
c dwnwrd.for    []
      subroutine dwnwrd(alrms, altla, altlo, altz,
     * rmslim, zdn, axz)
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /phoqn/ inst,knst
      real lonep, latep
      common /qmost1/ lonep,ni,latep
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      dimension zinc(8)
      data zinc/20, 20, 20, 20, 20, 20, 20, 20/
      inst = 1
c set initial steps according to erz estimate
      zbas = axz
      if(zbas .lt. .2) zbas = .2
      if(zbas .gt. 20.) zbas = 20.
      zinc(1) = zbas
      zinc(2) = zbas*2.
c find downward shift in z with respect to altz that results in rms = rmslim
      savla = altla
      savlo = altlo
      savez = altz + zinc(1)
      alrmsl = alrms
      altzl = altz
      n = 1
38    call quakes
      if(rms .ge. rmslim) then
        zdn = (savez - altzl)*(rmslim - alrmsl) /
     *          (rms - alrmsl) + altzl - altz
      else
        n = n + 1
        altzl = savez
        savez = savez + zinc(n)
        if(savez - altz .gt. 110.) then
          zdn = 99.
          return
        endif
        alrmsl = rms
        savla = latep
        savlo = lonep
        goto 38
      endif
      return
      end
c end dwnwrd
c eigen1.for    []
      subroutine eigen1(a, mev, mv, n, ev, s, v, damp)
c used to find eigenvalues and eigenvectors for the upper left nxn
c     portion of v(mv, mv)
c
c modified from an ibm sub. by j. c. lahr
c
c     dimension of r must be greater than n**2,
c     so n may not exceed 10 in this version
c              mev              (input) actual dimensions of ev
      integer  mev
c              mv               (input) actual dimensions of v
      integer  mv
c              n                (input) tensor dimension
      integer  n
c              a(n + (n*n-n)/2) input v transformed to storage mode 1
      real     a(n + (n*n-n)/2)
c              damp             (input) term to be added to diagonal elements
      real     damp
c              ev(mev, mev)     (output) eigenvectors
      real     ev(mev, mev)
c              r(100)           eigenvectors as linear array
      real     r(100)
c              s(n)             (output) eignevalues
      real     s(n)
c              v(mv, mv)        (input) symetric tensor to be diagonalized
      real     v(mv, mv)
c
      do 10 j = 1, n
      do 10 i = 1, j
        ni = i + (j*j-j)/2
        a(ni) = v(i, j)
        if (i .eq. j) a(ni) = a(ni) + damp
10    continue
      iq = -n
      do 20 j = 1, n
        iq = iq+n
        do 20 i = 1, n
          ij = iq+i
          r(ij) = 0.0
          if (i-j) 20, 15, 20
15        r(ij) = 1.0
20    continue
      anorm = 0.0
      do 35 i = 1, n
      do 35 j = i, n
        if (i-j) 30, 35, 30
30      ia = i+(j*j-j)/2
        anorm = anorm+a(ia)*a(ia)
35    continue
      if (anorm) 165, 165, 40
40    anorm = 1.414*sqrt(anorm)
      anrmx = anorm*1.0e-6/float(n)
      ind = 0
      thr = anorm
45    thr = thr/float(n)
50    l = 1
55    m = l+1
60    mq = (m*m-m)/2
      lq = (l*l-l)/2
      lm = l+mq
      if ( abs(a(lm))-thr) 130, 65, 65
65    ind = 1
      ll = l+lq
      mm = m+mq
      x = 0.5*(a(ll)-a(mm))
      y = -a(lm)/ sqrt(a(lm)*a(lm)+x*x)
      if (x) 70, 75, 75
70    y = -y
75    sinx = y/sqrt(2.0*(1.0+(sqrt((1.-y)*(1.+y)))))
      sinx2 = sinx*sinx
      cosx = sqrt((1.-sinx)*(1.+sinx))
      cosx2 = cosx*cosx
      sincs  = sinx*cosx
      ilq = n*(l-1)
      imq = n*(m-1)
      do 125 i = 1, n
        iq = (i*i-i)/2
        if (i-l) 80, 120, 80
80      if (i-m) 85, 120, 90
85      im = i+mq
        goto 95
90      im = m+iq
95      if (i-l) 100, 105, 105
100     il = i+lq
        goto 110
105     il = l+iq
110     x = a(il)*cosx-a(im)*sinx
        a(im) = a(il)*sinx+a(im)*cosx
        a(il) = x
120     ilr = ilq+i
        imr = imq+i
        x = r(ilr)*cosx-r(imr)*sinx
        r(imr) = r(ilr)*sinx+r(imr)*cosx
        r(ilr) = x
125   continue
      x = 2.0*a(lm)*sincs
      y = a(ll)*cosx2+a(mm)*sinx2-x
      x = a(ll)*sinx2+a(mm)*cosx2+x
      a(lm) = (a(ll)-a(mm))*sincs+a(lm)*(cosx-sinx)*(cosx+sinx)
      a(ll) = y
      a(mm) = x
130   if (m-n) 135, 140, 135
135   m = m+1
      goto 60
140   if (l-(n-1)) 145, 150, 145
145   l = l+1
      goto 55
150   if (ind-1) 160, 155, 160
155   ind = 0
      goto 50
160   if (thr-anrmx) 165, 165, 45
165   iq = -n
      do 185 i = 1, n
        iq = iq+n
        ll = i+(i*i-i)/2
        jq = n*(i-2)
        do 185 j = i, n
          jq = jq+n
          mm = j+(j*j-j)/2
          if (a(ll)-a(mm)) 170, 185, 185
170       x = a(ll)
          a(ll) = a(mm)
          a(mm) = x
          do 180 k = 1, n
            ilr = iq+k
            imr = jq+k
            x = r(ilr)
            r(ilr) = r(imr)
180       r(imr) = x
185   continue
      do 190 j = 1, n
        ni = j + (j*j-j)/2
        s(j) = a(ni)
190   continue
      ni = 0
      do 200 i = 1, n
        do 200 j = 1, n
          ni = ni + 1
          ev(j, i) = r(ni)
200   continue
      return
      end
c end eigen1
c erset.for    [unix]
      subroutine erset (p, a, b, c, d)
      integer p
      logical a, b, c, d
c* (vax
c      call errset (p, a, b, c, d)
c* vax)
c* (pc
c* pc)
c* (unix
c* unix)
      return
      end
c end erset
c fmplot.for    []
      subroutine fmplot
c plot first-motion directions on the lower focal hemisphere
c------- in equal area projection
      include 'params.inc' 
      logical repeat
      real bat2,bon2,mag
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      common /hf/ cp(72),sp(72)
      common /imost/ test(100)
      common /obcfn/ ain(npa)
      common /ocfn/ adj,seh,lat1,bat2,lon1,bon2,igap
      character*1 isnla, isnlo, krm1, krm2
      common /ocfn1/ isnla, isnlo, krm1, krm2
      common /ofln/ sec
      common /ofnv/ kmin
      common /omnfh/ dmin,dmin3,sminp
      character msym*1
      common /pfo/ msym(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pno/ lph,keyph(npa),nsum
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      common /qmost/ wt(npa),z
      common /xfmno/ mag
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
c
c     for multics printer use xscale = 0.101064
c     for slac printer use xscale = 0.1379
      xscale = test(45)
      repeat = .false.
      if (test(7) .lt. 0.) repeat = .true.
      write(punt,2)
    2 format(52h1  date    origin    lat n    long w    depth    mag,
     * 17h no gap dmin  rms)
      rmag = mag
      if (mag .eq. blank) rmag = 0.0
      write(punt,5) kdate,khr,kmin,sec,lat1,bat2,lon1,bon2,krm1,
     * z-test(8),krm2,rmag,nrwt,igap,dmin,rms
    5 format(2x,i6.6,1x,2i2,f6.2,i3,1h-,f5.2,i4,1h-,f5.2,a1,f6.2,a1
     *,f6.2,i3,i4,f5.1,f5.2)
      call fmplt(msta, az, ain, msym, lph, xscale, repeat, punt,
     *                 cp, sp)
      return
      end
c end fmplot
c fmplt.for    []
      subroutine fmplt(msta, az, ain, fm, lph, xscale, repeat, punt,
     *                 cp, sp)
c plot first-motion directions on the lower focal hemisphere
c------- in equal area projection
      include 'params.inc' 
      integer punt
      character*1 fm(npa), jtem, igrap(95, 95)
      character*5 msta(npa)
      dimension az(npa), ain(npa)
      dimension cp(72), sp(72)
      logical repeat
      data nox, noy, iy, noy1, noy2/95, 59, 24, 57, 30/
      data rmax, yscale, add/3.937008, 0.169643, 4.75/
      ntin = 0
    6 ntin = ntin + 1
      nfmr = 0
c zero graph
      do 10 i = 1, nox
      do 10 j = 1, noy
   10 igrap(i, j) = ' '
c make circle of *'s
      do 20 i = 1,72
        jx = (rmax*cp(i)+add)/xscale + 1.5
        jy = (rmax*sp(i)+add)/yscale + 0.5
        jy = noy - jy - 1
        ii = i - (i/2)*2
        igrap(jx, jy) = '*'
        if (ii .eq. 0) igrap(jx, jy) = ' '
   20 continue
      nox2 = add/xscale + 1.5
      it = (-rmax + add)/xscale + 1.5
      igrap(it, noy2) = '-'
      it = (rmax + add)/xscale + 2.5
      igrap(it, noy2) = '-'
      it = noy2 - iy - 1
      igrap(nox2, it) = 'i'
      it = noy2 + iy + 1
      igrap(nox2, it) = 'i'
c     center is nox2, noy2
      igrap(nox2, noy2) = '*'
      do 50 i = 1, lph
        if (fm(i) .eq. ' ') goto 50
        if (ain(i) .gt. 90.) goto 30
        ann = ain(i)
        azz = az(i)*.0174533
        goto 32
   30   ann = 180. - ain(i)
        azz = (180. + az(i) )*.0174533
   32   r = rmax*1.414214*sin(ann*.0087266)
        x = r*sin(azz) + add
        y = r*cos(azz) + add
        jx = x/xscale + 1.5
        jy = y/yscale + .5
        jy = noy - jy - 1
        if (ntin .eq. 2) goto 52
        jtem = igrap(jx, jy)
c if previous symbol is weak, overwrite it.
        if ((jtem.eq.' ').or.(jtem.eq.'*').or.(jtem.eq.'+')
     *  .or.(jtem.eq.'-').or.(jtem.eq.' ').or.(jtem.eq.'?')) goto 47
c if new symbol is weak, do not overwrite strong symbol.
        if ((fm(i).eq.'+').or.(fm(i).eq.'-').or.(fm(i).eq.'?'))
     *  goto 50
        if (fm(i) .eq. 'c' ) goto 40
c plot d on top of previous strong symbol
        if (igrap(jx, jy) .ne. 'd') goto 35
        igrap(jx, jy) = 'e'
        goto 50
   35   if (igrap(jx, jy) .ne. 'e') goto 37
        igrap(jx, jy) = 'f'
        goto 50
   37   if (igrap(jx, jy) .eq. 'f') goto 50
        igrap(jx, jy) = 'x'
        goto 50
c plot c on top of privious strong symbol
   40   if (igrap(jx, jy) .ne. 'c') goto 43
        igrap(jx, jy) = 'b'
        goto 50
   43   if (igrap(jx, jy) .ne. 'b') goto 45
        igrap(jx, jy) = 'a'
        goto 50
   45   if (igrap(jx,jy) .eq. 'a') goto 50
        igrap(jx, jy) = 'x'
        goto 50
   47   igrap(jx, jy) = fm(i)
        goto 50
c write station name on focal sphere
   52   njx = jx - 1
        do 56 j = 1, 4
          if (msta(i)(j:j) .ne. ' ') igrap(njx, jy) = msta(i)(j:j)
          njx = njx + 1
   56   continue
   50 continue
      nm1 = noy1 - 1
      do 80 i = 4, nm1
        write(punt, 55) (igrap(j, i), j = 1, nox)
   55   format(21x, 95a1)
   80 continue
      if ((repeat) .and. (ntin .eq. 1)) then
        write(punt, 82)
   82   format(1h1, /)
        goto 6
      endif
      return
      end
c end fmplt
c fold2.for    []
      subroutine fold2(alat,alon,la,ins,ala,lo,iew,alo)
c
c-------- given geographic coordinates compute geocentric lat and lon
c
c input:  la     degree portion of latitude in degrees
c         ins    n for north, s for south
c         ala    minutes portion of latitude
c         lo     degree portion of longitude
c         iew    e for east, w for west
c         alo    minutes portion of longitude
c output: alat   geocentric latitude in radians
c         alon   longitude in radians
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
      parameter (c1 = (1.0 - flat)**2)
      parameter (c2 = halfpi*(1.0/c1 - 1.0))
c
      character*1 ins, iew, dnstrg
c
      alat = (la + ala*1.6666667e-2)*rad
c ggtogc - convert from geographic to geocentric latitude
      if (halfpi-abs(alat) .ge. 0.02) goto 201
         alat = alat/c1-sign(c2,alat)
         goto 202
  201    alat = atan(c1*tan(alat))
  202    continue
      if (dnstrg(ins) .eq. 's') alat = -alat
      alon = (lo + alo*1.6666667e-2)*rad
      if (dnstrg(iew) .eq. 'e') alon = -alon
      return
      end
c end fold2
c formal.for    []
      subroutine formal(x, ix, n, nsig, fmit, xout)
c test program for subroutine formal
c      character*6 fmit
c      print *, 'test formal'
c30    n = iaskk ('give number of columns for number', n)
c      print *, 'format f3.1 uses 1 decimal digit.'
c      nsig = iaskk ('give number of decimal digits in read statement',
c     1nsig)
c      x = raskk ('give number to be printed out', x)
c      call formal(x, ix, n, nsig, fmit, xout)
c      if (fmit .eq. ' ') then
c        print *, 'write ', ix, ' with format i',n
c      else
c        print *, 'write ', xout, ' with format ', fmit
c      endif
c      goto 30
c      end
c---- converts real number x into the integer ix to be
c---- written with format(in) and read with format(fn.nsig)
c---- corrections by willy aspinall, principia testing  july 1983
c
c---- if the number is too large to write as an integer and
c---- n - nsig is greater than 1, then find xout and fmit
c---- so that the number may be written as xout with fmit.
c
      character*6 fmit
      ix = x*(10.**nsig) + sign(0.50001,x)
      imax = 10**n
      fmit = '      '
      if(x .gt. 0.) then
c       positive number
        if(ix .ge. imax) then
          if(nsig .lt. 2) then
            ix = imax - 1
            return
          else
c           in this case pass back a format to be used in writing
            call formit(x, xout, fmit, n, 1)
            return
          endif
        endif
        if(ix .le. imax/100) then
          if((n - nsig) .gt. 1) then
c           in this case pass back a format to be used in writing
            call formit(x, xout, fmit, n, 1)
            return
          endif
        endif
      else
c       negative number
        if(ix .le. -imax/10) then
          if(nsig .lt. 2) then
            ix = -imax/10 + 1
            return
          else
c           in this case pass back a format to be used in writing
            call formit(x, xout, fmit, n, 1)
            return
          endif
        endif
        if(ix .ge. -imax/1000) then
          if((n - nsig) .gt. 2) then
c           in this case pass back a format to be used in writing
            call formit(x, xout, fmit, n, 1)
            return
          endif
        endif
      endif
      end
c end formal
c formf.for    []
      subroutine formf(x, ix, n, nsig)
c converts real number x into the integer ix to be
c written with format(in) and read with format(fn.nsig)
c corrections by willy aspinall, principia testing  july 1983
      ix = x*(10.**nsig) + sign(0.50001,x)
      imax = 10**n
      if(ix .ge. imax) ix = imax - 1
      if(ix .le. -imax/10) ix = -imax/10 + 1
      return
      end
c end formf
c formit.for    []
      subroutine formit(a, aout, fmit, n, ifix)
c        squeez a positive real number (a) into a field of
c        length n where 2<n<9, and maintain the maximum number of
c        significant digits.
c     input: n (integer) and a (real).
c            ifix = 0, never change input number
c            ifix .ne. 0, if number is out of range, set equal to
c            maximum or minimum number that can be printed.
c     output: best format (fmit) of form (fn.x) (a6)
c             aout is the number to be printed
c
      character*6 fmit
      character*1 int(10), ilem(4)
      data int/'0','1','2','3','4','5','6','7','8','9'/
c
      ilem(1) = 'f'
      ilem(2) = int(n+1)
      ilem(3) = '.'
      nm1 = n - 1
      nm2 = n - 2
      aout = a

      if(ifix .eq. 0) go to 20

      top = 10.**nm1 - 0.5
      bot = -(10.**nm2) + 0.5
      if(aout .ge. top) aout = top - .1
      if(aout .le. bot) aout = bot + .1

20    if(aout .eq. -0.) then
        i = n - 1
      else if(aout .eq. 0.) then
	i = n
      else if(aout .lt. 0.) then
        do 30 i = 1,nm2
          nm = nm2 - i
          ip1 = i + 1
          if(aout .le. -(10.**nm - 5./10.**ip1)) go to 50
30      continue
        i = nm1
      else
        do 40 i = 1,nm1
          nm = nm1 - i
          ip1 = i + 1
          if(aout .ge. (10.**nm - 5./10.**ip1)) go to 50
40      continue
        i = n
      endif
50    ilem(4) = int(i)

      write(fmit, 70) ilem
70    format('(', 4a1, ')')
      return
      end
c end formit
c getbin.f    []
      subroutine getbin (input_unit, max_records,
     &  max_points, nrecords, station_list, amp_source, begin_date,
     &  end_date, npoints, period_sysmag)

      integer   i, j
      integer   input_unit, max_records, max_points, nrecords
      character station_list(max_records)*(*)
      character amp_source(max_records)*1
      integer   begin_date(*), end_date(*), npoints(*)
      real      period_sysmag(2,max_points,max_records)

      i = 1

100   read (input_unit,end=999) station_list(i), amp_source(i),
     &  begin_date(i), end_date(i), npoints(i),
     &  (period_sysmag(1,j,i), period_sysmag(2,j,i), j=1, npoints(i))
      i = i + 1
      goto 100

999   nrecords = i - 1
      return
      end
c end getbin
c geterr.for    []
      subroutine geterr(zup, zdn, rmslim)
c this subroutine will find the error limits for depth.
c j. c. lahr 4/22/87
      include 'params.inc' 
      parameter (ndly = 11)
      real bat2,bon2,lat,lon
      logical good, eoff, supout
      common /gg/ altla(3), altlo(3), altz(3), altrms(3), nsolut,
     * fla(20), flo(20), fz(20), frms(20), forg(20), maxf, altorg(3)
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /char/ iahead, msta(npa), nsta(nsn), icard
      character*4 ipro, ichec, evtype*1, evstat*1, root*256
      common /dbhip/ ipro, ichec, evtype, evstat, root
      common /dmost/ ipun,ivlr,blank
      common /dhil/ iq,ilat,kms,icat
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /dhio/ nedit,good
      common /dio/ rmsmx,presmx,sresmx,noutmx,nimx,iprun,semx
      common /dph/ noswt, eoff
      common /ghnq/ iexit
      common /gmost/ az(npa)
      common /hf/ cp(72),sp(72)
      common /hl/ nwad, tslope, tsqsl
      common /hpn/ wslope
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /iclmpq/ lat(nsn),lon(nsn)
      common /logfil/ logfil
      common /idno/ ksel,ksort
      common /ilv/ c(nsn), e(nsn)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /lm/ mapend
      common /ocfn/ adj,seh,lat1,bat2,lon1,bon2,igap
      character*1 isnla, isnlo, krm1, krm2
      common /ocfn1/ isnla, isnlo, krm1, krm2
      common /ohq/ gap, supout
      common /omnfh/ dmin,dmin3,sminp
      common /on/ ix,iy,iz,ax1,axz
      common /ph/ nfirst
      common /pno/ lph,keyph(npa),nsum
      character*4 krms
      common /po/ krms(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pgqv/ w(npa)
      common /phoqn/ inst,knst
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pnoqtx/ ldx(npa)
      character*4 krmp
      common /pnoqv/ krmp(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /qmost/ wt(npa),z
      real latep,lonep
      common /qmost1/ lonep,ni,latep
      common /ro/ yse,seorg,phi
      common /reloc/ irelo, nreloc
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      common /zqr/ fno
c
c given a local minimum, find the depth limits within which the
c rms will not increase by more than expected, given yse, the
c reading standard error.
c
c set iprn = -2 to supress printing
      isvprn = iprn
      iprn = -2
      rmslim = sqrt(altrms(1)**2 + (yse**2)/fno)
c
c first consider the case of only one solution
      if(nsolut .eq. 1) then
c
c
        call upward(altrms(1), altla(1), altlo(1), altz(1),
     *  frms(1), rmslim, zup, axz)
c
        call dwnwrd(altrms(1), altla(1), altlo(1), altz(1),
     *  rmslim, zdn, axz)
c
      else
c next consider the case of two solutions
        if(altz(1) .lt. altz(2)) then
c in this case, the primary solution is more shallow.
c get the upper limit above the more shallow solution.
          call upward(altrms(1), altla(1), altlo(1), altz(1),
     *    frms(1), rmslim, zup, axz)
c
c get the lower limit below the deeper solution.
          call dwnwrd(altrms(2), altla(2), altlo(2), altz(2),
     *    rmslim, zdn, axz)
          zdn = zdn + (altz(2) - altz(1))
        else
c in this case, the secondary solution is more shallow.
c get the upper limit above the more shallow solution.
          call upward(altrms(2), altla(2), altlo(2), altz(2),
     *    frms(1), rmslim, zup, axz)
          zup = zup + (altz(1) - altz(2))
c
c get the lower limit below the deeper solution.
          call dwnwrd(altrms(1), altla(1), altlo(1), altz(1),
     *    rmslim, zdn, axz)
        endif
c
      endif
      iprn = isvprn
      return
      end
c end geterr
c getsta.for    []
      subroutine getsta(   indexs, nsta, ielv, mod,
     *         ipthk, vthk, ipdly, dly, sdly, lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr, ns,
     *         prr, inpt, nreloc, inmain, injump, exdly, ibate, 
     *         test53)
c cole sonafrank notes of august, 1988:
c restructured the code a bit while trying to follow the logic...
c first reading of station list for parameters in effect on ibate
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (irecsz = 132)
      integer punt
      common /punt/ punt
      real lat,lon
      dimension ielv(nsn),nsta(nsn),dly(ndly,nsn),mod(nsn),ipthk(nsn)
      dimension vthk(2,nsn),ipdly(nsn),sdly(ndly,nsn),lat(nsn),lon(nsn)
      dimension c(nsn),e(nsn),sw(nsn),ndate(nsn),nhr(nsn),klas(5, nsn)
      dimension calr(5, nsn),fmgc(nsn),xmgc(nsn),ipcod(nsn),iscod(nsn)
      integer fmwt, xmwt
      dimension fmwt(nsn),xmwt(nsn),tpdly(2,nsn),prr(nsn)
      character*1 exdly(4, nsn), revp(6, nsn)
      character icard*(irecsz), icardup*(irecsz), dnstrg*(irecsz)
      character*4 nsta4, nsta*5, name, iast*1, rshft, nsta5
      character*1 ins, iew, izne
      integer iostat
      l = 1
      logfil = punt
c     write(logfil, '(a)') 'begin getsta.for - unit logfil'
  312 read(inpt, '(a)', end=999) icardup
      icard = dnstrg(icardup)
      name = rshft(icard(1:4))
      if(name(1:2) .eq. 'c*') goto 312
      if(name .eq. 'jump') then
        if(inpt .eq. inmain) then
          inpt = injump
c	  write (logfil, '(2a)') 'jump to station list: ', icardup(6:55)
          call openfl( injump, icardup(6:55), 'old', 'zero', 'readonly',
     *    'none', 0)
          goto 312
        else
          write(punt, 319) icard(1:55)
  319     format(' xxxerrorxxx can not nest jump statements ', /, 1x, a)
          stop
        endif
      endif
c
c ---   read primary station data values.
  320 continue
      if(nreloc .eq. 0) then
c        write (logfil,
c    *   '('' reading primary station values.  nreloc='',i2)')
c    *    nreloc
c        write (logfil, '(1x, a)') icard
        read(icard, 325)
     *    nsta4,llat,ins,alat,llon,iew,alon,ielv(l),
     *    mod(l),ipthk(l),vthk(1,l),vthk(2,l),ipdly(l),(dly(i,l),
     *    sdly(i,l),i=1,5), izne
  325   format(a4,i2,a1,f5.3,1x,i3,a1,f5.3,i5,i2,i1,2f4.2,i1,
     *    10f4.2,a1)
c ---   shift name to right and add component
	nsta(l)(1:4) = rshft(nsta4)

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!


        if(test53 .eq. 1.0) then
          if(nsta4(1:1) .ne. ' ') then
            if((nsta4(4:4) .eq. 'n') .or. (nsta4(4:4) .eq. 'e')) then
              izne = nsta4(4:4)
            endif
          endif
        endif

	if((izne .ne. 'e') .and. (izne .ne. 'n')) then
	  nsta(l)(5:5) = 'z'
	else
	  nsta(l)(5:5) = izne
	endif
        
c zero the extra delay models
        do 3255 i = 6, ndly-1
          dly(i,l) = 0.0
          sdly(i,l) = 0.0
3255    continue
      else
c        write (logfil,
c    *  '('' reading primary values without delays.  nreloc='', i2)')
c    *  nreloc
c ---   do not read delays if job is now recycling.
        read(icard, 326)
     *  nsta4,llat,ins,alat,llon,iew,alon,ielv(l),
     *  mod(l),ipthk(l),vthk(1,l),vthk(2,l),ipdly(l),izne
  326   format(a4,i2,a1,f5.3,1x,i3,a1,f5.3,i5,i2,i1,2f4.2,i1,40x,a1)
c ---   shift name to right and add component
	nsta(l)(1:4) = rshft(nsta4)

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!

        if(test53 .eq. 1.0) then
          if(nsta4(1:1) .ne. ' ') then
            if((nsta4(4:4) .eq. 'n') .or. (nsta4(4:4) .eq. 'e')) then
              izne = nsta4(4:4)
            endif
          endif
        endif

	if((izne .ne. 'e') .and. (izne .ne. 'n')) then
	  nsta(l)(5:5) = 'z'
	else
	  nsta(l)(5:5) = izne
	endif
      endif
c --- just read primary record
      if (nsta(l)(1:4) .eq. ' end') goto 991
      if (mod(l) .lt. 1) mod(l) = 1
      prr(l) = 0.1
      call fold2(lat(l),lon(l),llat,ins,alat,llon,iew,alon)
      if (l .eq. 1) then
        c(1) = 0.0
        e(1) = 0.0
      else
        call delaz(lat(1),lon(1),delt,deldeg,azz,lat(l),lon(l))
        c(l) = delt*sin(azz*rad)
        e(l) = delt*cos(azz*rad)
      endif
      if (ipthk(l) .ne. 2) ipthk(l) = 1
      if ((ipdly(l) .lt. 1) .or. (ipdly(l) .gt. 10)) ipdly(l) = 1
c
c --- read time dependent station data.
330   read(inpt, '(a)', iostat=iostat) icard
      icard = dnstrg(icard)
      if(icard(1:2) .eq. 'c*') goto 330
      if (iostat.eq.-1) then
        if(inpt .eq. injump) then
          inpt = inmain
          icard(1:4) = ' end'
          goto 390
        else
          goto 999
        endif
      endif
c decode a time dependent station record (icard) for station number l
      if(icard(5:5) .ne. '*') goto 395
      call rdtmdp(l, icard, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr,
     *         exdly)
      if (ndate(l) .lt. ibate) goto 350
c
c --- station not expired, so read next record
  340 read(inpt, '(a)', iostat=iostat) icard
      icard = dnstrg(icard)
      if(icard(1:2) .eq. 'c*') goto 340
c --- shift name to right 
      nsta4 = rshft(icard(1:4))
      if (iostat.eq.-1) then
        if(inpt .eq. injump) then
          inpt = inmain
          icard(1:4) = ' end'
          iast = ' '
          goto 990
        else
          goto 999
        endif
      endif
      iast = icard(5:5)
      izne = icard(80:80)
c     write(logfil, '(a, 1x, a, 1x, a, 1x, i5)') 
c    *  nsta4, iast, nsta(l), l
      if (icard(1:4) .eq. ' end') goto 990
      if ((iast .ne. '*') .or. (nsta4 .ne. nsta(l)(1:4))) then
c --- this is NOT another time-dependent record
	if (iast .ne. '*') then
c         this SHOULD be a new station

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!

          if(test53 .eq. 1.0) then
            if(nsta4(1:1) .ne. ' ') then
              if((nsta4(4:4) .eq. 'n') .or. (nsta4(4:4) .eq. 'e')) then
                izne = nsta4(4:4)
              endif
            endif
          endif

	  if((izne .ne. 'e') .and. (izne .ne. 'n')) then
	    nsta5 = nsta4//'z'
	  else
	    nsta5 = nsta4//izne
	  endif
          if (nsta5 .ne. nsta(l))  then
c           this IS a new station
            goto 360
          else
c	    the same station AGAIN!  A clear error.
	    goto 392
	  endif
	else
c	  it is an error to have an * with a different station name
	  goto 392
	endif
      endif
c --- write extra time dependent record
      write(2,'(2a)') nsta(l), icard(6:lentru(icard))
      goto 340

c --- station expired so far, look for unexpired one.
  350 continue
c     write (logfil, '('' station expired so far'')')
      read(inpt, '(a)', iostat=iostat) icard
      icard = dnstrg(icard)
c     write (logfil,'(x,a)') icard
      if(icard(1:2) .eq. 'c*') goto 350
      nsta4 = rshft(icard(1:4))
      if (iostat.eq.-1) then
        if (inpt .eq. injump) then
          inpt = inmain
          goto 991
        else
          goto 999
        endif
      endif
      iast = icard(5:5)
      izne = icard(80:80)
      if (nsta4 .eq. ' end') goto 991
      if ((iast .ne. '*') .or. (nsta4 .ne. nsta(l)(1:4))) then
c --- this is NOT another time-dependent record
	if (iast .ne. '*') then
c         this SHOULD be a new station

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!

          if(test53 .eq. 1.0) then
            if(nsta4(1:1) .ne. ' ') then
              if((nsta4(4:4) .eq. 'n') .or. (nsta4(4:4) .eq. 'e')) then
                izne = nsta4(4:4)
              endif
            endif
          endif

	  if((izne .ne. 'e') .and. (izne .ne. 'n')) then
	    nsta5 = nsta4//'z'
	  else
	    nsta5 = nsta4//izne
	  endif
          if (nsta5 .ne. nsta(l))  then
c           this IS a new station
            goto 320
          else
c	    the same station AGAIN!  A clear error.
	    goto 393
	  endif
	else
c	  it is an error to have an * with a different station name
	  goto 392
	endif
      endif
      if(icard(5:5) .ne. '*') goto 395
      call rdtmdp(l, icard, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr,
     *         exdly)
      if (ndate(l) .lt. ibate) goto 350

c --- found non-expired data for this one, save extra data.
      goto 340

  360 if (indexs .gt. 0) call iprst(l,llat,ins,alat,llon,iew,alon)
      l = l + 1
c     write (logfil, '('' 360: l='',i3)') l
      if (l .le. nsn) goto 320
      write(punt,370) nsn
  370 format(///,' xxxerrorxxx  station list exceeds max of ', 
     *  i5, ' stations', /, ' so stop.')
      stop
  390 write(punt,380) l,nsta(l),name,iast
  380 format(///,'  xxxerrorxxx 390 in station list format, so stop',/,
     *  1x,i5,'th station ',2('"',a5,'" '),a1)
      stop
  392 write(punt,382) l, nsta(l), name, iast
  382 format(///,'  xxxerrorxxx 392 in station list format, so stop',/,
     *  1x,i5,'th station ',2('"',a5,'" '),a1)
      stop
  393 write(punt,383) l,nsta(l),name,iast
  383 format(///,'  xxxerrorxxx 393 in station list format, so stop',/,
     *  1x,i5,'th station ',2('"',a5,'" '),a1)
      stop
  394 write(punt,384) l,nsta(l),name,iast
  384 format(///,'  xxxerrorxxx 394 in station list format, so stop',/,
     *  1x,i5,'th station ',2('"',a5,'" '),a1)
      stop
  395 write(punt, 385) l, nsta(l), name, iast
  385 format(///,'  xxxerrorxxx in station list format, so stop',/,
     *  1x,i5,'th station ',2('"',a5,'" '),a1, 
     *  ' missing time-dependent record')
 990  if (indexs .gt. 0) call iprst(l,llat,ins,alat,llon,iew,alon)
      l = l + 1
      if (iast .eq. '*') goto 394
 991  ns = l - 1
      write(2, 992)
 992  format(' end')
      return
 999  write(punt, 389)
 389  format (' xxxerrorxxx unexpected end of file on station list')
      stop 'error - getsta'
      end
c end getsta
c global.for    []
      subroutine global
c this subroutine attempt to find the global minimum when there are
c more than one local minimum at different depths.  j. c. lahr 4/22/87
c
      include 'params.inc' 
      parameter (ndly = 11)
      real bat2,bon2,lat,lon
      logical good, eoff, supout
      integer punt
      common /punt/ punt
      common /gg/ altla(3), altlo(3), altz(3), altrms(3), nsolut,
     * fla(20), flo(20), fz(20), frms(20), forg(20), maxf, altorg(3)
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /char/ iahead, msta(npa), nsta(nsn), icard
      character*4 ipro, ichec, evtype*1, evstat*1, root*256
      common /dbhip/ ipro, ichec, evtype, evstat, root
      common /dmost/ ipun,ivlr,blank
      common /dhil/ iq,ilat,kms,icat
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /dhio/ nedit,good
      common /dio/ rmsmx,presmx,sresmx,noutmx,nimx,iprun,semx
      common /dph/ noswt, eoff
      common /ghnq/ iexit
      common /gmost/ az(npa)
      logical ingulf
      common /gu/ ingulf
      common /hf/ cp(72),sp(72)
      common /hl/ nwad, tslope, tsqsl
      common /hpn/ wslope
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /iclmpq/ lat(nsn),lon(nsn)
      common /logfil/ logfil
      common /idno/ ksel,ksort
      common /ilv/ c(nsn), e(nsn)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /ihfgpq/ itest(100)
      common /lm/ mapend
      common /ocfn/ adj,seh,lat1,bat2,lon1,bon2,igap
      character*1 isnla, isnlo, krm1, krm2
      common /ocfn1/ isnla, isnlo, krm1, krm2
      common /ohq/ gap, supout
      common /omnfh/ dmin,dmin3,sminp
      common /ph/ nfirst
      common /pno/ lph,keyph(npa),nsum
      character*4 krms
      common /po/ krms(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pgqv/ w(npa)
      common /phoqn/ inst,knst
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pnoqtx/ ldx(npa)
      character*4 krmp
      common /pnoqv/ krmp(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /qgo/ org
      common /qmost/ wt(npa),z
      real latep,lonep
      common /qmost1/ lonep,ni,latep
      common /ro/ yse,seorg,phi
      common /reloc/ irelo, nreloc
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      common /zqr/ fno
c
      ingulf = .false.
c
c deep depth should be deep enough to catch deepest local mimima.
c we use 75. km in alaska.  add test(8) so that deepz is in km wrt
c the top of the model, not wrt sea level.
c     test(42) = 75. is the default.
      deepz = test(42) + test(8)
c
c a solution is first computed for z = 0 and for z free, starting at deepz.
c
c if the deep solution converges to a depth less than some cutoff,
c cutz, then the solution with smaller rms is taken as the best.
c we use 20 in alaska.  add test(8) so that deepz is in km wrt
c the top of the model, not wrt sea level.
c     test(27) = 20. is the default value
      cutz = test(27) + test(8)
c
c if the deep solution converges to a depth greater than cutz, then
c another solution is found starting at shalz = cutz/2.0.
c     shalz = 9. (old value)   new default value is 10.
      shalz = cutz/2.0
c
c emperical tests indicate that for alaska data this algorithm with
c the default values of test(27) and test(42) usually
c finds the global minimum.  for other networks, different values of
c deepz, cutz, and shalz may need to be used.
c
      if(iprn .ge. 5) then
        write(punt,91)
   91   format(' call global')
c show elapsed time
        call timit(1)
      endif
      instsv = inst
      if(instsv .eq. 8) freor = .false.
c set inst = 1 to fix depth
      inst = 1
c set savez to surface
      savez = 0.
      call quakes
c save this location and rms
      fla(1) = latep
      flo(1) = lonep
      fz(1) = z
      frms(1) = rms
      forg(1) = org
      rmslm1 = frms(1)
      if(fno .gt. 0.) rmslm1 = sqrt(frms(1)**2 + (yse**2)/fno)
c set savez to deep depth
      savez = deepz
      call quakes
c save this location and rms
      fla(2) = latep
      flo(2) = lonep
      fz(2) = z
      frms(2) = rms
      forg(2) = org
c now free up the depth and begin at deep depth again
      inst = 0
      savla = latep
      savlo = lonep
      call quakes
c if solution is in the gulf of alaska, then save it and return
      if(ingulf) then
	if(iprn .ge. 5) then
          write(punt, '(a)') 
     *    'Global deep-starting free solution ends up in gulf.'
        endif
        altla(1) = latep
        altlo(1) = lonep
        altz(1) = z
        altrms(1) = rms
        altorg(1) = org
        nsolut = 1
        return
      endif
c save this location and rms
      fla(3) = latep
      flo(3) = lonep
      fz(3) = z
      frms(3) = rms
      forg(3) = org
      rmslm3 = frms(3)
      if(fno .gt. 0.) rmslm3 = sqrt(frms(3)**2 + (yse**2)/fno)
      if(fz(3) .lt. cutz) then
c the solution must be shallow.
        maxf = 3
        if(fz(3) .le. 0.1) then
c in this case there is only one minimum, and it is at the surface
          altla(1) = fla(3)
          altlo(1) = flo(3)
          altz(1) = fz(3)
          altrms(1) = frms(3)
          altorg(1) = forg(3)
          nsolut = 1
          return
        else if(rmslm1 .lt. frms(3)) then
c in this case near the surface is significantly better,
c so run free location to derive information for output
          savez = fz(1)
          savla = fla(1)
          savlo = flo(1)
          inst = 0
          call quakes
c save this location and rms
          altla(1) = latep
          altlo(1) = lonep
          altz(1) = z
          altrms(1) = rms
          altorg(1) = org
          nsolut = 1
          return
        else if(frms(1) .gt. rmslm3) then
c the deeper solution is significantly better, so use it
          altla(1) = latep
          altlo(1) = lonep
          altz(1) = z
          altrms(1) = rms
          altorg(1) = org
          nsolut = 1
          return
        else
c in this case the difference in rms is less than limit
c these will be joined if there is not a boundary in between them.
          nsolut = 2
c the prefered solution will have the lesser rms
          if(frms(1) .le. rms) then
c the surface is at least slightly better or equal to deeper z.
c save current solution as secondary minimum.
            altla(2) = latep
            altlo(2) = lonep
            altz(2) = z
            altrms(2) = rms
            altorg(2) = org
c recompute information for output.
            savez = fz(1)
            savla = fla(1)
            savlo = flo(1)
            if (instsv .ne. 8) freor = .true.
            inst = 9
            call quakes
c save this location and rms
            altla(1) = latep
            altlo(1) = lonep
            altz(1) = z
            altrms(1) = rms
            altorg(1) = org
            return
          else
c the deeper solution is at least slightly better.
            altla(1) = latep
            altlo(1) = lonep
            altz(1) = z
            altrms(1) = rms
            altorg(1) = org
            altla(2) = fla(1)
            altlo(2) = flo(1)
            altz(2) = fz(1)
            altrms(2) = frms(1)
            altorg(2) = forg(1)
            return
          endif
        endif
      else
c the final depth is greater than cutz km when starting at deep depth
        altla(1) = latep
        altlo(1) = lonep
        altz(1) = z
        altrms(1) = rms
        altorg(1) = org
c compute a fixed depth solution at shallow depth
        savez = shalz
        savla = fla(1)
        savlo = flo(1)
        inst = 1
        call quakes
c save this location and rms
        fla(4) = latep
        flo(4) = lonep
        fz(4) = z
        frms(4) = rms
        forg(4) = org
        maxf = 4
c compare rms at surface and shallow depth and start free solution at better.
        if(frms(4) .le. frms(1)) then
c shallow depth is better than surface so start free solution there.
          inst = 0
          savla = latep
          savlo = lonep
          call quakes
c save this location and rms
          fla(5) = latep
          flo(5) = lonep
          fz(5) = z
          frms(5) = rms
          forg(5) = org
          rmslm5 = frms(5)
          if(fno .gt. 0.) rmslm5 = sqrt(frms(5)**2 + (yse**2)/fno)
          maxf = 5
        else
c surface is better than shallow depth so start free soltuion there.
          savez = fz(1)
          savla = fla(1)
          savlo = flo(1)
          inst = 0
          call quakes
c save this location and rms
          fla(5) = latep
          flo(5) = lonep
          fz(5) = z
          frms(5) = rms
          forg(5) = org
          rmslm5 = frms(5)
          if(fno .gt. 0.) rmslm5 = sqrt(frms(5)**2 + (yse**2)/fno)
          maxf = 5
        endif
c       if(abs(fz(5) - fz(3)) .lt. 5.) then
        if(abs(fz(5) - fz(3)) .lt. deepz/10.) then
c the depths are so close together that these solutions are equivalent.
          nsolut = 1
          if(frms(5) .le. frms(3)) then
            altla(1) = fla(5)
            altlo(1) = flo(5)
            altz(1) = fz(5)
            altrms(1) = frms(5)
            altorg(1) = forg(5)
            return
          else
c compute fixed depth solution for output values
            savla = fla(3)
            savlo = flo(3)
            savez = fz(3)
            if (instsv .ne. 8) freor = .true.
            inst = 9
            call quakes
c save this location and rms
            altla(1) = latep
            altlo(1) = lonep
            altz(1) = z
            altrms(1) = rms
            altorg(1) = org
            return
          endif
        else
c the depths are far enough appart that there may be two solutions.
          if(frms(5) .gt. frms(3)) then
c the deep solution is at least slightly better
            altla(1) = fla(3)
            altlo(1) = flo(3)
            altz(1) = fz(3)
            altrms(1) = frms(3)
            altorg(1) = forg(3)
            altla(2) = fla(5)
            altlo(2) = flo(5)
            altz(2) = fz(5)
            altrms(2) = frms(5)
            altorg(2) = forg(5)
            nsolut = 1
            savla = fla(3)
            savlo = flo(3)
            savez = fz(3)
            if (instsv .ne. 8) freor = .true.
            inst = 9
            call quakes
          else
c the shallow solution is at least slightly better
            altla(2) = fla(3)
            altlo(2) = flo(3)
            altz(2) = fz(3)
            altrms(2) = frms(3)
            altorg(2) = forg(3)
            altla(1) = fla(5)
            altlo(1) = flo(5)
            altz(1) = fz(5)
            altrms(1) = frms(5)
            altorg(1) = forg(5)
          endif
          if(frms(5) .gt. rmslm3) then
c the deep solution is significantly better
            nsolut = 1
            return
          else if(frms(3) .gt. rmslm5) then
c the shallow solution is significantly better
            nsolut = 1
            return
          else
c report two solutions
            nsolut = 2
            return
          endif
        endif
      endif
      end
c end global
c glob_new.for    []
      subroutine glob_new
c this subroutine attempt to find the global minimum when there is
c more than one local minimum at different depths.  j. c. lahr 4/22/87
c
      	include 'params.inc' 
      	parameter (ndly = 11)
      	integer punt
      	common /punt/ punt
      	common /gg/ altla(3), altlo(3), altz(3), altrms(3), nsolut,
     * fla(20), flo(20), fz(20), frms(20), forg(20), maxf, altorg(3)
      	logical ingulf
      	common /gu/ ingulf
      	logical freor
      	common /hopq/ savla,savlo,savez,savor,freor
      	common /igl/ nglobalzs, globalzs(20)
      	common /imost/ test(100)
      	common /ihfgpq/ itest(100)
      	common /phoqn/ inst,knst
      	common /qmost/ wt(npa),z
      	real latep,lonep
      	common /qmost1/ lonep,ni,latep
      	common /ro/ yse,seorg,phi
      	common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      	common /zqr/ fno

c
c deep depth should be deep enough to catch deepest local mimima.
c we use 75. km in alaska.  add test(8) so that deepz is in km wrt
c the top of the model, not wrt sea level.

      	i = nglobalzs
	
20	savez = globalzs(i) + test(8)
	write(punt, *) 'savez =', savez
      	instsv = inst
      	if(instsv .eq. 8) freor = .false.
c set inst = 1 to fix depth
      	inst = 1
      	call quakes

c free up the depth
	inst = 0
      	savla = latep
      	savlo = lonep
	savez = z
	write(punt, *) 'savez for free run is ', savez
	call quakes

	write(punt, *) 'z =', z

c save this depth and rms
      	fz(i) = z
      	fla(i) = latep
      	flo(i) = lonep
        frms(i) = rms
c     	rmslm1 = frms(1)
c     	if(fno .gt. 0.) rmslm1 = sqrt(frms(1)**2 + (yse**2)/fno)

c for now, just force a start at every depth specified
	if (i .gt. 1) then
	  i = i - 1
	  goto 20
	endif

c this version would allow depths to be skipped
c	if (i .gt. 1) then
c	  find next depth to test
c	  do j = i - 1, 1, -1
c	    if (fz(i) .gt. globalzs(j)) then
c	      i = j
c	      goto 20
c	    else
c	      skip this starting depth
c	      frms(j) = 999999.
c	      fz(j) = -99.
c	    endif
c	  enddo
c	endif

c now decide which was the best solution
	write(punt, *) frms
	write(punt, *) fz
	bestrms = frms(1)
	ibest = 1
	do i = 2, nglobalzs
	  if (frms(i) .lt. bestrms) then
	    bestrms = frms(i)
	    ibest = i
	  endif
	enddo

c get ready to make final run again
	savela = fla(ibest)
	savelo = flo(ibest)
	savez = fz(ibest)
	inst = instsv
	return 
	end

c halfsp.for    []
      subroutine halfsp(m, t, tz, vh, x, xz, y, yz, zz)
      common /logfil/ logfil
c solve for best halfspace solution, given m p arrival times and the
c origin time.
c              mx               maximum number of equations (m .eq. mx)
      parameter (mx = 10)
c              a(6)             storage mode 1 version of sum of squares
      real     a(6)
c              m                (input) number of p arrival times
      integer  m
c              c(4, mx)         normal equation coeficients and constants
      real     c(4, mx)
c              ev(3, 3)         eigenvector matrix
      real     ev(3, 3)
c              r(3)             normal equation constants in rotated system
      real     r(3)
c              s(3)             eignevalues
      real     s(3)
c              t(mx)            (input) arrival time at station m
      real     t(mx)
c              tz               origin time
      real     tz
c              vh               halfspace velocity
      real     vh
c              v(4, 4)          symetric tensor  - upper 3x3 to be diagonalized
      real     v(4, 4)
c              x(mx)            (input) x coordinate of station m
      real     x(mx)
c              xyt(3)           eq coordinates in original system
      real     xyt(3)
c              xytr(3)          eq coordinates in rotated system
      real     xytr(3)
c              xz               (output) x coordinate of earthquake
      real     xz
c              y(mx)            (input) y coordinate of station m
      real     y(mx)
c              yz               (output) y coordinate of earthquake
      real     yz
c              zz               (output) z coordinate of earthquake
      real     zz
c
      if (m .gt. mx) then
        print *, 'halfsp can not have more than ', mx, ' equations,'
        print *, 'so ', m, ' is too many.'
        stop
      endif
      n = 3
      vsq = vh*vh
      nfix = 1
      if (tz .eq. 99999.) nfix = 0
c
c set up cooeficients of equations, c
c
cd    print *, 'original equations for ', m, ' arrival times:'
      do 20 i = 1, m-1
        c(1, i) = x(m) - x(i)
        c(2, i) = y(m) - y(i)
        if (tz .eq. 99999.) then
c         in this case, tz is unknown
          c(3, i) = -vsq*(t(m) - t(i))
          c(4, i) = ( x(m)*x(m) + y(m)*y(m) - t(m)*t(m)*vsq
     *             -x(i)*x(i) - y(i)*y(i) + t(i)*t(i)*vsq )/2.
        else
c         in this case, tz is given
          c(3, i) = ( x(m)*x(m) + y(m)*y(m) - t(m)*t(m)*vsq
     *             -x(i)*x(i) - y(i)*y(i) + t(i)*t(i)*vsq )/2.
     *             +vsq*( t(m) - t(i) )*tz
          c(4, i) = 0.0
        endif
cd      print *, i, (c(j, i), j = 1, 4)
20    continue
c
c compute sum of squares matrix from coeficients
c
      do 25 j = 1, 4-nfix
        do 25 k = j, 4-nfix
          v(j, k) = 0.0
          do 23 i = 1, m-1
23          v(j, k) = v(j, k) + c(j, i)*c(k, i)
25    v(k,j) = v(j,k)
cd    print *, 'normal equations:'
cd    do 26 j = 1, 4-nfix
cd      print *, (v(j, k), k = 1, 4-nfix)
cd26   continue
c
c compute determinant
c
cd    det = deter(v, 4, n-nfix)
cd    print *, 'determinant = ', det
c
c compute eigenvalues and eigenvectors of nxn tensor v
c ev is the nxn eigenvector tensor
      call eigen1(a, 3, 4, n-nfix, ev, s, v, 0.0)
c
cd    write (logfil, *) ' eigenvectors'
cd    do 45 i = 1, 3
cd      write (logfil, *) (ev(i, j), j = 1, 3)
cd45   continue
cd    write (logfil, *) ' eigenvalues'
cd    write (logfil, *) s
c
c
c compute response vector in rotated coordinates
c
      do 30 j= 1, n-nfix
        r(j) = 0.0
        do 30 k = 1, n-nfix
          r(j) = r(j) + ev(k, j)*v(k, 4-nfix)
30    continue
cd    print *, 'response vector in rotated coordinates'
cd    print *, r
c
c compute the eq location in rotated system
c
      do 50 i = 1, n-nfix
        if(s(i) .gt. .000001) then
          xytr(i) = r(i)/s(i)
        else
          xytr(i) = 0.0
        endif
50    continue
c
c rotate location back into original coordinates
c
      do 55 j = 1, n-nfix
        xyt(j) = 0.0
        do 55 k = 1, n-nfix
55        xyt(j) = xyt(j) + ev(j,k)*xytr(k)
c
c define output variables
c
      xz = xyt(1)
      yz = xyt(2)
      if (tz .eq. 99999.) tz = xyt(3)
      arg =  vsq*(t(m) - tz)**2 - (x(m) - xz)**2 - (y(m) - yz)**2
      if (arg .ge. 0.) then
        zz = sqrt (arg)
      else
cd      print *, 'arg = ', arg, ' so z set to zero'
        zz = 0.
      endif
cd    print *, 'xz        yz        zz          tz'
cd    print *, xz, yz, zz, tz
c
      return
      end
c end halfsp
c hycrt.for    []
      subroutine hycrt (mod, itable, vpvs)
c    written by f. klein for hypoinverse
c    modified slightly for hypoellipse - j.c. lahr
c    further improvements - j.a. snoke - january 1991 - spherical earth
c--reads a travel time table for a linear gradient crust model
c--called by hypoinv
c--data used only for linear gradient travel time tables
c--ln is number of models
      parameter (ln=3)
c  Changed nlyr from 12 to 20 on 2/9/2000 jcl
      parameter (nlyr=20)
      integer punt
      common /punt/ punt
      character modnam*20
      common /mc/ modnam(ln)
c                            model name or label
      common /m/ lay(ln)
c                            number of layers or v-d points
      common /m/ d(nlyr,ln)
c                            depth to layer top or velocity point
      common /m/ vel(nlyr,ln)
c                            layer or point velocity
      common /m/ thk(nlyr,ln)
c                            thickness of homogeneous layer
      common /m/ vsq(nlyr,ln)
c                            squared velocity of homogeneous layer
      common /m/ modtyp(ln)
c                            mdl type (-1=undef, 0=grad, 1=homo layer)
c                            the number of linear-grad models allowed
      integer kt
      logical gd1,gd2,gz1,gz2
      common /m/ redv(ln)
c                            one over the reducing velocity
      common /m/ nz(ln)
c                            number of depth grid points
      common /m/ nz1(ln),dz1(ln),nz2(ln),dz2(ln)
c                            depth grid params
      common /m/ nd(ln)
c                            number of distance grid points
      common /m/ nd1(ln),dd1(ln),nd2(ln),dd2(ln)
c                            dist grid params
      common /m/ gd1(ln),gd2(ln),gz1(ln),gz2(ln)
c                            grid flags
      common /m/ kdhr(ln,28)
c                            for each model (ln) and for each depth,
c                            the distance at which horizontal ray emerges
c                            (in units of 0.1 km)
      common /m/ kt(ln,28,42)
c                            the travel times (up to 28 z's & 42 dist's)
c--identify the model mod as a linear gradient table
      modtyp(mod)=0
c
c--read header info
      rewind(itable)
      read (itable,1000) modnam(mod),lay(mod),redv(mod),vpvs,radius
1000  format (a20, i5, 2f10.5, f10.3)
      write(punt, '(1x, a20, 5x, a, f10.5)')
     *  modnam(mod), 'vp/vs =', vpvs
c
c--read depths & velocities of model
      read (itable,1001) (d(i,mod),i=1,lay(mod))
      read (itable,1001) (vel(i,mod),i=1,lay(mod))
1001  format (3x, 15f7.2)
c
c--read distance & depth grid info
      read (itable,1003) dd1(mod),nd1(mod),dd2(mod),nd2(mod)
      read (itable,1003) dz1(mod),nz1(mod),dz2(mod),nz2(mod)
1003  format (3x, 2(f10.4, i5))
      write(punt, '('' z '', 15f7.2)') (d(i, mod), i = 1, lay(mod))
      write(punt, '('' v '', 15f7.2)') (vel(i, mod), i = 1, lay(mod))
      write(punt, '('' dd '', 2(f10.4, i5))')
     *  dd1(mod), nd1(mod), dd2(mod), nd2(mod)
      write(punt, '('' dz '', 2(f10.4, i5))')
     *  dz1(mod), nz1(mod), dz2(mod), nz2(mod)
      nd(mod)=nd1(mod)+nd2(mod)+1
      nz(mod)=nz1(mod)+nz2(mod)+1
c
c--read reduced travel times, grouped by depth
      do 20 j=1,nz(mod)
      read (itable,1004) kdhr(mod,j)
1004  format (20x,i10)
20    read (itable,1005) (kt(mod,j,i),i=1,nd(mod))
1005  format (15i8)
      gd1(mod)=nd1(mod).ne.0 .and. dd1(mod).ne.0
      gd2(mod)=nd2(mod).ne.0 .and. dd2(mod).ne.0
      gz1(mod)=nz1(mod).ne.0 .and. dz1(mod).ne.0
      gz2(mod)=nz2(mod).ne.0 .and. dz2(mod).ne.0
      return
      end
c end hycrt
c hypot.for    []
      real function hypot(a, b)
c
c-------- bruce julian
c
c
c-------- hypot - calculates euclidian distance, accurately and
c            avoids overflow
c
      real a, b
      real abs, l, s, t, sqrt
      l = abs(a)
      s = abs(b)
      if (s .le. l) goto 1
         t = s
         s = l
         l = t
   1  if (l .ne. 0.0) goto 2
         hypot = 0.0
         return
   2  s = s/l
      hypot = l*sqrt(s*s+1.0)
      return
      end
c end hypot
c hyset.for    []
      subroutine hyset(nttab, z1)
c    written by f. klein for hypoinverse
c    modified slightly for hypoellipse - j.c. lahr
c    further improvements - j.a. snoke - january 1991 - spherical earth
c--initialize values for a given depth, z1.
c--uses a condensed and reduced travel time table generated
c--by the program ttgen.
      parameter (ln=3)
c                            the number of linear-grad models allowed
c  Changed nlyr from 12 to 20 on 2/9/2000 jcl
      parameter (nlyr=20)
      character modnam*20
      common /mc/ modnam(ln)
c                            model name or label
      common /m/ lay(ln)
c                            number of layers or v-d points
      common /m/ d(nlyr,ln)
c                            depth to layer top or velocity point
      common /m/ vel(nlyr,ln)
c                            point velocity
      common /m/ thk(nlyr,ln)
c                            thickness of homogeneous layer
      common /m/ vsq(nlyr,ln)
c                            squared velocity of homogeneous layer
      common /m/ modtyp(ln)
c                            mdl type (-1=undef, 0=grad, 1=homo layer)
c--data used only for linear gradient travel time tables
      integer kt
      logical gd1,gd2,gz1,gz2
      common /m/ redv(ln)
c                            one over the reducing velocity
      common /m/ nz(ln)
c                            number of depth grid points
      common /m/ nz1(ln),dz1(ln),nz2(ln),dz2(ln)
c                            depth grid params
      common /m/ nd(ln)
c                            number of distance grid points
      common /m/ nd1(ln),dd1(ln),nd2(ln),dd2(ln)
c                            dist grid params
      common /m/ gd1(ln),gd2(ln),gz1(ln),gz2(ln)
c                            grid flags
      common /m/ kdhr(ln,28)
c                            for each model (ln) and for each depth,
c                            the distance at which horizontal ray emerges
c                            (in units of 0.1 km)
      common /m/ kt(ln,28,42)
c                            the travel times (up to 28 z's & 42 dist's)
      common /tlook/ vh(ln)
c                            velocity at hypocenter
      common /tlook/ dhrz(ln)
c                            dist at which horiz ray emerges
      common /tlook/ tz(ln,42)
c                            tt at each distance point
      common /tlook/ dtz(ln,42)
c                            tt depth deriv at each distance point
      data scfac/2000./
c--loop over the ln possible models, skipping loop if model not used.
      do 35 md=1,nttab
c--perform some preliminary calcs and interpolations which depend only on
c  depth. do this for each linear grad model once.
      nzm=nz(md)
      nz1m=nz1(md)
      nz2m=nz2(md)
      dz1m=dz1(md)
      dz2m=dz2(md)
      laym=lay(md)
      nd1m=nd1(md)
c--perform depth interpolation first
c--find depth index = index of table entry nearest hypocenter
c--also depth spacing h and fraction of interval x
      temp=nz1m*dz1m
      if ((.not.gz1(md) .or. z1.gt.temp) .and. gd2(md)) then
c--hypo in lower part of table
        i=nz1m+(z1-temp)/dz2m+1.5001
        if (i.ge.nzm) i=nzm-1
        if (i.lt.nz1m+2) i=nz1m+2
        h=dz2m
        x=(z1-temp)/dz2m-(i-nz1m-1)
        i1 = nz1m + (z1-temp)/dz2m + 1.0001
        if (i1.ge.nzm) i1 = nzm - 1
        y = (z1 - temp)/dz2m - (i1 - nz1m - 1)
      else
c--hypo in upper part of table
        i=z1/dz1m+1.5001
        if (i.gt.nz1m) i=nz1m
        if (i.lt.2) i=2
        h=dz1m
        x=z1/dz1m-(i-1)
        i1 = z1/dz1m + 1.0001
        y = z1/dz1m - (i1 - 1)
      end if
c--find exact velocity at hypocenter
      l = 1
      do 20 k=1,laym
      if (d(k,md).lt.z1) l=k
20    continue
      vh(md)=vel(laym,md)
      if (l.lt.laym) vh(md)=vel(l,md)+(vel(l+1,md)-vel(l,md))
     *   *(z1-d(l,md))/(d(l+1,md)-d(l,md))
c--interpolate dist at which a horizontal ray emerges
      dhrz(md)=(kdhr(md,i1)+y*(kdhr(md,i1+1)-kdhr(md,i1)))*.1
c--depth interpolation
      if (z1.le.temp+dz2m*nz2m) then
c--use 3 point interpolation
        ca=x*.5*(x-1.)
        cb=1.-x**2
        cc=x*.5*(x+1.)
        da=(x-.5)/h
        db=-2.*x/h
        dc=(x+.5)/h
      else
c--use linear extrapolation
        ca=0.
        cb=1.-x
        cc=x
        da=0.
        dc=1./h
        db=-dc
        dhrz(md)=1000.
      end if
c--interpolate tt and its depth derivative for all distance grid points
      temp=nd1m*dd1(md)
      do 30 j=1,nd(md)
      dx=(j-1)*dd1(md)
      if (j.gt.nd1m+1) dx=temp+(j-nd1m-1)*dd2(md)
      tz(md,j)=(ca*kt(md,i-1,j)+cb*kt(md,i,j)
     * +cc*kt(md,i+1,j)+32000.)/scfac+dx*redv(md)
30    dtz(md,j)=(da*kt(md,i-1,j)+db*kt(md,i,j)+dc*kt(md,i+1,j))/scfac
35    continue
      return
      end
c end hyset
c hytab.for    []
      subroutine hytab(md, dx, travti, ain, dtdd, dtdh, vt, vs)
c    written by f. klein for hypoinverse
c    modified slightly for hypoellipse - j.c. lahr
c    further improvements - j.a. snoke - january 1991 - spherical earth
c--md is model for this station
c--dx is station distance
      parameter (ln=3)
c                            the number of linear-grad models allowed
c
c  Changed nlyr from 12 to 20 on 2/9/2000 jcl
      parameter (nlyr=20)
      character modnam*20
      common /mc/ modnam(ln)
c                            model name or label
      common /m/ lay(ln)
c                            number of layers or v-d points
      common /m/ d(nlyr,ln)
c                            depth to layer top or velocity point
      common /m/ vel(nlyr,ln)
c                            point velocity
      common /m/ thk(nlyr,ln)
c                            thickness of homogeneous layer
      common /m/ vsq(nlyr,ln)
c                            squared velocity of homogeneous layer
      common /m/ modtyp(ln)
c                            mdl type (-1=undef, 0=grad, 1=homo layer)
c--data used only for linear gradient travel time tables
      integer kt
      logical gd1,gd2,gz1,gz2
      common /m/ redv(ln)
c                            one over the reducing velocity
      common /m/ nz(ln)
c                            number of depth grid points
      common /m/ nz1(ln),dz1(ln),nz2(ln),dz2(ln)
c                            depth grid params
      common /m/ nd(ln)
c                            number of distance grid points
      common /m/ nd1(ln),dd1(ln),nd2(ln),dd2(ln)
c                            dist grid params
      common /m/ gd1(ln),gd2(ln),gz1(ln),gz2(ln)
c                            grid flags
      common /m/ kdhr(ln,28)
c                            for each model (ln) and for each depth,
c                            the distance at which horizontal ray emerges
c                            (in units of 0.1 km)
      common /m/ kt(ln,28,42)
c                            the travel times (up to 28 z's & 42 dist's)
      common /tlook/ vh(ln)
c                            velocity at hypocenter
      common /tlook/ dhrz(ln)
c                            dist at which horiz ray emerges
      common /tlook/ tz(ln,42)
c                            tt at each distance point
      common /tlook/ dtz(ln,42)
c                             tt depth deriv at each distance point
c--now find travel time, dtdr, dtdz, and angle of emergence for
c--start distance interpolation
      ndm=nd(md)
      nd1m=nd1(md)
      nd2m=nd2(md)
      dd1m=dd1(md)
      dd2m=dd2(md)
      temp=nd1m*dd1m
c--find distance index = index of table entry nearest the station dist
c--also distance spacing h and fraction of interval x
      if ((.not.gd1(md) .or. dx.gt.temp) .and. gd2(md)) then
c--hypo in far part of table
        j=nd1m+(dx-temp)/dd2m+1.5001
        if (j.ge.ndm) j=ndm-1
        if (j.lt.nd1m+2) j=nd1m+2
        h=dd2m
        x=(dx-temp)/dd2m-(j-nd1m-1)
      else
c--hypo in near part of table
        j=dx/dd1m+1.5
        if (j.gt.nd1m) j=nd1m
        if (j.lt.2) j=2
        h=dd1m
        x=dx/dd1m-(j-1)
      end if
c--distance interpolation
      if (dx.le.temp+dd2m*nd2m) then
c--use 3 point interpolation
        anin=x*.5*(x-1.)
        cb=1.-x**2
        cc=x*.5*(x+1.)
        da=x-.5
        db=-2.*x
        dc=x+.5
      else
c--use linear extrapolation
        anin=0.
        cb=1.-x
        cc=x
        da=0.
        dc=1.
        db=-1.
      end if
c--interpolate tt and its 2 derivatives
      travti=anin*tz(md,j-1)+cb*tz(md,j)+cc*tz(md,j+1)
      dtdh  =anin*dtz(md,j-1)+cb*dtz(md,j)+cc*dtz(md,j+1)
      dtdd=(da*tz(md,j-1)+db*tz(md,j)+dc*tz(md,j+1))/h
      if (dtdd.lt.0.) dtdd=0.
c--calc emergence angle
      anin=vh(md)*dtdd
      if (anin.gt..99999) anin=.99999
      ain=57.29578*asin(anin)
      if (dx.lt.dhrz(md)) ain=180.-ain
      if (dx.ge.dhrz(md)) anin = -anin
      vt = vel(1, md)
      vs = vh(md)
      return
      end
c end hytab
c init.for    [unix]
      subroutine init
c* (pc
c$notruncate
c* pc)
c get filenames, open files and write greeting for hypoellipse
      include 'params.inc' 
      character*256 root
      common /dhip/ inpt,isa,ilis,inmain,injump
      character*4 ipro, ichec, evtype*1, evstat*1
      common /dbhip/ ipro, ichec, evtype, evstat, root
      common /hf/ cp(72),sp(72)
      real*8 time1, time2
      common /hop/ time1,time2,nopu,notim
      common /it/ vpvs(lmax2),vpvsm(mmax+3),nttab
      common /lm/ mapend
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      integer punt
      common /punt/ punt
      integer print_unit, summary_unit, arch_unit
      parameter (print_unit   = 9)
      parameter (summary_unit = 4)
      parameter (arch_unit    = 11)
      character*1 print_type, summary_type, arch_type
      parameter (print_type   = 'o')
      parameter (summary_type = 's')
      parameter (arch_type    = 'a')
      character openstat*7
      openstat = 'unknown'
c
c open the files for hypoellipse (returns root name)
        call opfls(inpt, inmain, root, nttab)
        call timit(0)
c get current date
c       call jdate(irmo, irdy, iryr, ihr, imn, isec)
        irmo=1
        irdy=1
        iryr=1
        ihr=19
        imn=50
        isec=20
        
c do not print output conversion errors
        call erset (63,.true.,.false.,.false.,.false.)
        write(punt, 20) nsn, npa, iryr, irmo, irdy, ihr, imn
20      format (
c* (pc
c     *' *** Hypoellipse: PC/Non-Xpick/Y2K version 3.9 2/11/2000  ***',
c* pc)
c* (unix
     *' *** Hypoellipse: Unix/Non-Xpick/Y2K version 3.9 2/11/2000  ***',
c* unix)
     */, '  Configured for up to ', i4, ' stations in station list',
     */, '  and up to ', i4, ' records per earthquake.',
     */, '  Run on ', 2(i2.2, '/'), i2.2, ' at ', i2.2, ':', i2.2)
c calculate tables to be used in focal mechanism plots
        do 30 i=1,72
c         pi=i*.0349066*2.5
          pi = i*0.0872665
          cp(i)=cos(pi)
          sp(i)=sin(pi)
30      continue
        ichec = 't s '
        mapend=0
        nr = 0
        time1 = 0.0d0
      return
      end
c end initial
c input1.for    []
      subroutine input1
c input station list, crustal model, test and control values
c     to change max number of stations from nsn to some other number,
c       151 for example:
c                1)  change all occurrences of nsn) to 151)
c                2)  change nmax from nsn to 151 in next data statement.
c                3)  note:  be sure the new dimension is a unique number
c                      which has not been previously used so that
c                      subsequent changes in dimension will not be
c                      difficult to make.
c --- added inmain in place of the constant 8.  its now in the /dhip/
c --- common block and also passed to getsta().
      include 'params.inc' 
      parameter (ndly = 11)
      real lat,lon,latr,lonr
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      character*4 iscan
      character*4 iahead*60, msta*5, nsta*5, icard*110, dnstrg*110
      character*50 uacal, uaini, dlyfil, icardup*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dmost/ ipun,ivlr,blank
      common /dhil/ iq,ilat,kms,icat
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /dbiln/ ioldq
      character*1 iclass
      common /dbio/ iclass(0:4)
      common /dhin/ iglob, zup, zdn
      common /dhio/ nedit,good
      common /dio/ rmsmx,presmx,sresmx,noutmx,nimx,iprun,semx
      character*1 bksrc
      common /dipu/ ipkdly, igsum, bksrc
      logical medmag
      common /dinx/ imag,imagsv,medmag
      common /dit/ tid(lmax,lmmax),did(lmax,lmmax),lowv,modin(mmax+3)
      common /dix/ iexcal, uacal
      common /igl/ nglobalzs, globalzs(20)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /idt/ v(lmax2)
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn),xmwt(nsn)
      common /logfil/ logfil
      common /ihfgpq/ itest(100)
      character*1 iqcls
      common /il1/ iqcls
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /ilmpu/ ns
      common /iclmpq/ lat(nsn),lon(nsn)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /in/ irmo,iryr,odmag
      character*1  magke
      common /in1/ magke
      common /idno/ ksel,ksort
      common /iot/ flt(2,nsn),thks(nsn)
      common /iox/ prr(nsn),iuses
      common /iiq/ wf(51)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
      common /ip1/ rsew(4)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /ilv/ c(nsn),e(nsn)
      common /it/ vpvs(lmax2),vpvsm(mmax+3),nttab
      character*4 mtyp*2
      common /iu/ mtyp(mmax)
      common /ix/      ir,qspa(9,40)
      common /lvz/ jref(lmax)
      common /reloc/ irelo, nreloc
      common /rioq/ been,damp,dmpinc,igo
      character*4 inote(100)
      dimension atest(100),d(lmax2)
c initialize test variables used throughout the program
      character*1 ins, iew
      integer iostat
      data d/lmax2*0./
      data atest/1.78, 5.0, 0.0, 0.0, -99.0, 0., 10., 0., 0., 50.,
     * 50.0,100.0 ,50.0,50.0, 10.0,50.0, 2.0, 50.0, 0.05,0.05,
     * 9.0, 35., 0.7, 35., 40., 0.0025, 20.0, 0., -.1, 0.0, 
     * -1.15, 2.00, 0.0, 0.0, 0.001, 100.0, 3.0, 0.0, 1.0, 0.007, 
     * 0.0, 75.0, 2*0., 0.1379, 0.0, 0.0, 6.5, 2*0., 1000., 
     * 2800., 1.0, 200., 19., 45*0.0/
      data inote/100*'    '/
      data uaini/' '/, kl/0/
      data nglobalzs/0/, globalzs/20*0.0/
      fsteq = .true.
200   continue
      nkrst = 0
      been = 0.0
205   read(inpt, '(a)', iostat = iostat) icardup
      icard = dnstrg(icardup)
      if (iostat.eq.-1) then
        if (ilis.gt.0) then
          write(punt, 220) inpt
          write(logfil, 220) inpt
220       format(' subroutine input1 found end of file on unit ', i5)
        endif
        if(inpt .eq. inmain) then
          write(logfil, 225)
          write(punt, 225)
225       format(' xxxerrorxxx abnormal termination while ',
     *    'reading control file.')
          stop 'abort from input1'
        else
c         return to reading from unit inmain
          inpt = inmain
          goto 205
        endif
      endif
      iscan = icard(1:4)
      if (icard .eq. ' ') goto 205
      if (iscan(1:4) .eq. 'stop') then
        write(punt, '(a)') ' stop processing with stop instruction'
        stop 'normal termination'
      endif
      if (iscan(1:2) .eq. 'c*') then
        write(punt, 235) icard
235     format(1x, a)
        goto 205
      endif
      if (iscan .eq. 'head') then
        iahead = icard(19:lentru(icard))
        if (ilis.gt.0) write(punt, '(//35x,a)') iahead
        goto 205
      endif
c
c remove everything on record beginning with an ! mark
      do 2515 i = 1, 110
        if(icard(i:i) .eq. '!') then
          if(i .eq. 1) goto 205
          icard = icard(1:i-1)
          goto 2517
        endif
2515  continue
c
2517  if (iscan .eq. 'arc ') then
        if(ilis .gt. 0)
     *  write(punt, '(a, a)')
     *    ' open archive file named:  ', icardup(19:78)
        close(11)
        call openfl(   11, icardup(19:78), 'unknown', 'null', 
     *  'none', 'noprint', 0)
        goto 205
      endif
c
      if (iscan .eq. 'uofa') then
        uacal = icardup(19:78)
        goto 205
      endif
      if (iscan .eq. 'jump') then
        if (inpt .eq. inmain) then
          close (unit = injump,iostat = iostat)
          if (ilis.gt.0) write(punt, '(1x, a)') icardup
          call openfl(injump, icardup(6:55), 'old', 'zero', 'readonly',
     *    'none', 0)
          inpt = injump
        else
          write(logfil, 260) inmain
          write(punt, 260) inmain
260       format(' xxxwarningxxx can not nest jumps from control file,'
     *    ,/, ' returning to main input file (unit ',i2,')')
          inpt = inmain
        endif
        goto 205
      endif
c because standard fortran 77 to can not read an internal file with
c free format, file 14 must be used in the following code!
      rewind 14
      write(14, '(a)') icard(19:80)
      rewind 14
      if (iscan .eq. 'rese') then
c       read(icard(19:80), *, err = 5243) j, ax
        read(14, *, iostat = iostat) j, ax
        if (iostat.ne.0) then
          ierror = 1
          goto 5243
        endif
        if (( j .gt. 100) .or. ( j .lt. 1 )) then
          ierror = 1
          goto 530
        endif
        inote(j) = '****'
        test(j) = ax
        goto 205
      endif
c
      if ((iscan .eq. 'crus') .or. (iscan .eq. 'velo')) then
c       read(icard(19:80), *, err = 5243)  ax, bx, cx
        read(14, *, iostat = iostat)  ax, bx, cx
        if (iostat.ne.0) then
          ierror = 2
          goto 5243
        endif
        if (nkrst .eq. 0) then
          lh = lmax + 2
          do  290 k = 1,lh
            d(k) = 0.0
            v(k) = -1.
290       continue
          do 295 k = 1, mmax
            mtyp(k) = 'ps'
295       continue
        endif
        nkrst = nkrst + 1
        if (nkrst .gt. lmax) then
          write(logfil, 310) icard
          write(punt, 310) icard(1:30)
310       format(' too many velocity layers. ',
     *    ' layer ', a, ' was not used.')
        else
          v(nkrst) = ax
          d(nkrst) = bx
          vpvs(nkrst) = cx
        endif
        goto 205
      endif
c
      if (iscan .eq. 'weig') then
c       read(icard(19:80), *, err = 5243) ax, bx, cx
        read(14, *, iostat = iostat) ax, bx, cx
        if (iostat.ne.0) then
          ierror = 3
          goto 5243
        endif
        if ((ax .lt. 1.).or.(bx .lt. 1.).or.(cx .lt. 1.)) then
          if (ilis.gt.0) write(punt, 320) icard
320       format(' weight option parameters represent the ratio of', /,
     *    ' the standard error of weight code 1, 2, and 3 ', /,
     *    ' readings to the standard error of 0 weight code', /,
     *    ' readings.  none can be less than 1. so stop.', /,
     *    1x, a)
          stop 'abort from input1.'
        endif
        rsew(2) = ax
        rsew(3) = bx
        rsew(4) = cx
        goto 205
      endif
c
      if (iscan .eq. 'vari') then
c       read(icard(19:80), *, err = 5243) ivlr, ivway, lowv
        read(14, *, iostat = iostat) ivlr, ivway, lowv
        if (iostat.ne.0) then
          ierror = 4
          goto 5243
        endif
        goto 205
      endif
c
      if (iscan .eq. 'miss') then
c       read(icard(19:80), *, err = 5243) kms
        read(14, *, iostat = iostat) kms
        if (iostat.ne.0) then
          ierror = 5
          goto 5243
        endif
        if(ilis .gt. 0) then
          if (kms .eq. 0) write(punt,335)
335       format(' scan for missing stations.  code = 0')
          if (kms .ne. 0) write(punt,340) kms
340       format(' do not scan for missing stations.  code = ', i3)
        endif
        goto 205
      endif
c
      if (iscan .eq. 'prin') then
c       read(icard(19:80), *, err = 5243) iprn
        read(14, *, iostat = iostat) iprn
        if (iostat.ne.0) then
          ierror = 6
          goto 5243
        endif
        if ((iprn .lt. -2) .or. (iprn .gt. 5)) then
          ierror = 6
          goto 530
        endif
        goto 205
      endif
c
      if ((iscan .eq. 'punc') .or. (iscan .eq. 'summ')) then
c       read(icard(19:80), *, err = 5243) ipun
        read(14, *, iostat = iostat) ipun
        if (iostat.ne.0) then
          ierror = 7
          goto 5243
        endif
        if ((ipun .lt. -4) .or. (ipun .gt. 4)) then
          ierror = 7
          goto 530
        endif
        goto 205
      endif
c
      if (iscan .eq. 'magn') then
c       read(icard(19:80), *, err = 5243) j
        read(14, *, iostat = iostat) j
        if (iostat.ne.0) then
          ierror = 8
          goto 5243
        endif
        iabj = iabs(j)
        if (((iabj .gt. 4) .and. (iabj .lt. 10)) .or.
     *       (iabj .gt. 14)) then
          ierror = 8
          goto 530
        endif
        imagsv = j
        imag = iabj
        medmag = .false.
        if(imag .gt. 9) then
          medmag = .true.
	  imag = imag - 10
        endif
        iuses = 0
        if (j .lt. 0) iuses = 1
        goto 205
      endif
c
      if ((iscan .eq. 'qual') .or. (iscan .eq. 'tabu')) then
c       read(icard(19:80), *, err = 5243) j
        read(14, *, iostat = iostat) j
        if (iostat.ne.0) then
          ierror = 9
          goto 5243
        endif
        if ((j.lt. -4 ).or.(j.gt. 4)) then
          ierror = 9
          goto 530
        endif
        ioldq = 0
        if (j .lt. 0) ioldq = 1
        iq = iabs(j)
        iqcls = iclass(iq)
        goto 205
      endif
c
365   if (iscan .eq. 'sort') then
c       read(icard(19:80), *, err = 5243) ksort
        read(14, *, iostat = iostat) ksort
        if (iostat.ne.0) then
          ierror = 10
          goto 5243
        endif
        if(ilis .gt. 0) then
          if (ksort .eq. 0) write(punt,370)
370       format(' sort stations by distance.  code = 0' )
          if (ksort .ne. 0) write(punt,375) ksort
375       format(' do not sort stations by distance.  code = ', i3)
        endif
        goto 205
      endif
c
      if (iscan .eq. 'stan') then
        write(punt,395)
395     format(46h change all program options to standard values )
        do 400 i = 1, 100
          inote(i) = '    '
          test(i) = 1.23456
400     continue
        do 405 i = mmax+1, mmax+3
          modin(i) = 0
405     continue
        bksrc = ' '
        iexcal = 0
        ns = 0
        rsew(2) = 5.
        rsew(3) = 10.
        rsew(4) = 20.
        igsum = 1
        ivlr = 0
        lowv = 1
        kms = 1
        ir = 0
        iprn = 1
        ilis = 1
        ipun = 0
        iuses = 0
        imagsv = 0
	medmag = .false.
        imag = 0
        iq = 2
        iqcls = iclass(2)
        ioldq = 0
        ksort = 0
        inpt = inmain
        icat = 1
        nedit = 0
        iprun = 1
        ipkdly = 1
        irelo = 0
        iglob = 1
        uacal = uaini
        nglobalzs = 0
	do i = 1, 20
	  globalzs(i) = 0.0
	enddo
        goto 205
      endif
      if (iscan .eq. 'blan') then
c source code to use if phase record has blank field
        do 412 i = 19, 80
          if(icard(i:i) .ne. ' ') goto 413
412     continue
413     bksrc = dnstrg(icard(i:i))
        if (ilis .gt. 0) write(punt, 414) bksrc
414     format(' arrival-time record blank source fields will be',
     *  ' assumed to be source "', a, '"')
        goto 205
      endif
c
      if (iscan .eq. 'comp') then
c       read(icard(19:80), *, err = 5243) ksel
        read(14, *, iostat = iostat) ksel
        if (iostat.ne.0) then
          ierror = 12
          goto 5243
        endif
        if (ilis .gt. 0) then
          if (ksel .ne. 0) write(punt,415)
415       format(42h do not compress printed output.  code = 1)
          if (ksel .eq. 0) write(punt,420) ksel
420       format(33h compress printed output.  code =,i3)
        endif
        goto 205
      endif
c
425   if (iscan .eq. 'igno') then
c       read(icard(19:80), *, err = 5243) igsum
        read(14, *, iostat = iostat) igsum
        if (iostat.ne.0) then
          ierror = 13
          goto 5243
        endif
        if (igsum .eq. 0) then
          if (ilis.gt.0) write(punt, 430) igsum
430       format(/,' ignore sum code = ', i5, /,
     *    ' (ignore starting locations on summary records)')
        else
          if (ilis.gt.0) write(punt, 435) igsum
435       format(/,' ignore sum code = ', i5, /,
     *    ' (use starting locations from summary records)')
        endif
        goto 205
      endif
c
      if (iscan .eq. 'cali') then
        if (ilis.gt.0) write(punt,445)
445     format(/' input extra calibration curves.')
c       read(icard(19:80), *, err = 5243) ir
        read(14, *, iostat = iostat) iexcal
        if (iostat.ne.0) then
          ierror = 14
          goto 5243
        endif
        if (iexcal .lt. 1 .or. iexcal .gt. 9) then
          write(punt, '(a, a, /, a)') ' error in: ', icard,
     *    ' the number of extra calibrations must be between 1 and 9'
          write(logfil, '(a, a, /, a)') ' error in: ', icard,
     *    ' the number of extra calibrations must be between 1 and 9'
          stop 'abort from input1'
        else
          do 460 i = 1,iexcal
            read(inpt,450) (qspa(i,k),k = 1,40)
450         format(20f4.2)
            ip8 = i + 8
            if (ilis.gt.0) write(punt,455) ip8,(qspa(i,k),k = 1,40)
455         format(/,' qspa(', i2, ') ', 20f5.2, /, 10x, 20f5.2)
460       continue
        endif
        goto 205
      endif
c
465   if (iscan .eq. 'begi') goto 540
      if (iscan .eq. 'arri') then
        if(inpt .eq. inmain) then
          goto 580
        else
          write(logfil, 470) inmain
          write(punt, 470) inmain
470       format(' xxxerrorxxx input data structure error,' /,
     *    ' the arrival times next record must be in main', /,
     *    ' input stream (unit ',i2,').')
          stop 'abort from input1'
        endif
      endif
c
      if (iscan .eq. 'debu') then
c       read(icard(19:80), *, err = 5243) nedit
        read(14, *, iostat = iostat) nedit
        if (iostat.ne.0) then
          ierror = 15
          goto 5243
        endif
        if (nedit .eq. 0) then
          if (ilis .gt. 0) write(punt,480) nedit
480       format(33h do not use debug option.  code =,i3)
          goto 205
        endif
        read(inpt,485) icard
485     format(a)
        if (ilis .gt. 0) write(punt,490) icard
490     format(' debug option record is',/1x,a)
        read(icard, 495) rmsmx,presmx,sresmx,noutmx,nimx,semx
495     format(5x,3(5x,f5.2),2(5x,i5),5x,f5.0)
        goto 205
      endif
      if (iscan .eq. 'cal ') then
        if (ilis.gt.0) write(punt, 496)
496     format(' xxxx warning xxxx  calibration option no longer valid')
        goto 205
      endif
      if (iscan .eq. 'resi') then
c       read(icard(19:80), *, err = 5243) iprun
        read(14, *, iostat = iostat) iprun
        if (iostat.ne.0) then
          ierror = 16
          goto 5243
        endif
        goto 205
      endif
      if (iscan .eq. 'sele') then
c       read(icard(19:80), *, err = 5243) ipkdly
        read(14, *, iostat = iostat) ipkdly
        if (iostat .ne. 0) then
          ierror = 17
          goto 5243
        endif
        if(ipkdly .le. 0) then
          if (ilis.gt.0) write(punt,515)
515       format(' select delays based on sub. usedly regions.')
          if(ipkdly .lt. 0) then
            read(inpt, '(a)') dlyfil
c           call openfl(iunit,  ifile, istat,  izero, ishr,
c    *      iform, irecl)
            call openfl(   17, dlyfil, 'old', 'null', 'none',
     *      'none', 0)
            if (ilis.gt.0) write(punt, 5155) dlyfil
5155        format(' delay cylinders defined in the file:', /, 1x, a)
          endif
        else
          write(punt,516)
516       format(' select delays based on closest station.')
        endif
        goto 205
      endif
      if (iscan .eq. 'relo') then
c       read(icard(19:80), *, err = 5243) irelo
        read(14, *, iostat = iostat) irelo
        if (iostat.ne.0) then
          ierror = 18
          goto 5243
        endif
        if (ilis.gt.0) write(punt,522) irelo
522     format(' relocate events ', i2, ' times, each time', /,
     *  ' updating the station delays for delay model 1.')
        goto 205
      endif
      if (iscan .eq. 'newg') then
	read(14, *, iostat = iostat) (globalzs(i), i = 1, 20)
	do i = 20, 1, -1
	  if(globalzs(i) .ne. 0.0) then
	    nglobalzs = i
	    goto 5225
	  endif
	enddo
	write(punt, *) 
     *    ' new global option record included with no non-zero depths'
	write(logfil, *) 
     *    ' new global option record included with no non-zero depths'
	goto 205
5225    write(punt, 5226) nglobalzs, (globalzs(i), i = 1, nglobalzs)
        write(logfil, 5226) nglobalzs, (globalzs(i), i = 1, nglobalzs)
5226	format(' running new global option with ', i3, 
     *    ' starting depths of ', /, 2x, 10f10.2)
	goto 205
      endif
      if (iscan .eq. 'glob') then
c       read(icard(19:80), *, err = 5243) iglob
        read(14, *, iostat = iostat) iglob
        if (iostat.ne.0) then
          ierror = 19
          goto 5243
        endif
        if (ilis .gt. 0) then
          if (iglob .eq. 0) then
            write(punt, 5241)
            write(logfil, 5241)
5241        format(' global minimum search turned on')
          else
            write(punt, 5242)
            write(logfil, 5242)
5242        format(' global minimum search turned off')
          endif
        endif
        goto 205
      endif
      if (iscan .eq. 'cons') then
        read(14, *, iostat = iostat) j
        if (iostat .ne. 0) then
          ierror = 21
          goto 5243
        endif
        ilis = j
        write(logfil, '(a, i5)') ' constants print = ', ilis
        goto 205
      endif
      if (iscan .eq. 'dela') then
c       read an additional delay model
        read(14, *, iostat = iostat) nmodel
        if (iostat .ne. 0) then
          ierror = 22
          goto 5243
        endif
        call adddly(nmodel)
        goto 205
      endif
5243  write(punt,525)  icard, ierror
      write(logfil,525)  icard, ierror
525   format(' xxxerrorxxx control record in improper format.',
     * /,1x, a,/' so stop (error #', i3, ')')
      stop 'abort from input1'
530   write(logfil,535) ierror, icard
      write(punt,535) ierror, icard
535   format(
     * ' this record skipped because j is improper value (error #',
     * i3,'). ', /, 1x, a)
      goto 205
c input station list
540   if (ilis.gt.0) write(punt,550) iahead
550   format(' list of stations available for these solutions',
     *   /,35x,a)
560   continue
c     read(icard(19:80), *, err = 5243) indexs, ibate
      read(14, *, iostat = iostat) indexs, ibate
      if(ibate .lt. 19000000) then
	write(logfil, *) 'xxxerrorxxx century not included on ',
     *    'begin station list record.  date = ', ibate
	write(punt, *) 'xxxerrorxxx century not included on ',
     *    'begin station list record.  date = ', ibate
	stop
      endif
      if (iostat.ne.0) then
        ierror = 20
        goto 5243
      endif
      write(punt, '(1x, a)') icard(1:80)
      ihrmn = 0
c indexs = 0 or 1 -> stations list (index option has been removed)
c negative indexs -> no print
      if (indexs .eq. 0) indexs = 1
      if (ilis.gt.0) then
        if (iabs(indexs) .eq. 1) write(punt,565) indexs
565     format(' station list code =',i5)
        if (iabs(indexs) .ne. 1) then
          write(punt,'(a, i5)')
     *    ' xxxerrorxxx station list code must be 1, not ', indexs
          write(logfil,'(a)')
     *    ' xxxerrorxxx station list code must be 1, not ', indexs
          stop 'abort from input1'
        endif
        write(punt,575) ibate
575     format(' set up for events starting on ',i8,/,
     * ' name latitude  longitude  elev p thickness p p  pdy1 sdy1',
     * '  pdy2 sdy2  pdy3 sdy3  pdy4 sdy4  pdy5 sdy5     calr xmgc',
     * ' mgwt fmgc wt ',/,
     * ' * continuation  record *      thk 1   2  mod dly          ',
     * '                                            sys          ',
     * '           ps',/,
     * '  polarity stawt teldy code altdy cnyrmody hr')
      endif
      rewind 2
      l = 1
      if (test(2) .eq. 1.23456) test(2) = atest(2)
      if (test(53) .eq. 1.23456) test(53) = atest(53)
c     read station list
c     write(logfil, '(a)') ' just about to call getsta - unit logfil'
      call getsta(   indexs, nsta, ielv, mod,
     *         ipthk, vthk, ipdly, dly, sdly, lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr, ns,
     *         prr, inpt, nreloc, inmain, injump, exdly, ibate,
     *         test(53))
      infil = 3
      iofil = 2
      goto 205
c----
580   continue
c write test variables
      if (ilis.gt.0) write(punt,595) iahead
595   format(1h ,35x,a)
c     call jdate(irmo, irdy, iryr, ihr, imn, isec)
        irmo=1
        irdy=1
        iryr=1
        ihr=19
        imn=50
        isec=20

      iseed = irmo*irdy*iryr + imn + isec
      dum = ran3(-iseed)
      do 600 i = 1, 55
        if (test(i).eq.1.23456) test(i) = atest(i)
        itest(i) = test(i) + sign(0.0005,test(i))
600   continue
      if(test(34) .ne. 0.0) test(34) = 1.0
      itest(38) = abs(test(38)) +0.0005
      if ((iglob .eq. 0) .and. (test(47) .ne. 0.0)) then
        write(punt, 590)
        write(logfil, 590)
590     format(' xxxerrorxxx the global option is not compatible with',/
     *         '   the option to fix the hypocenter on a plane. ',/
     *         '   either set test(47) to 0.0 or turn off the',
     *         ' global option')
        stop 'abort from input1'
      endif
c define jeffreys weighting funct.
      if (iprn .ge. 3) write(punt, 605) test(20)
605   format(/,47h jeffreys weighting funct         i      weight,
     *  7h   mu =,f10.3)
      do 615 i = 1,51
        wf(i) = (1.0 + test(20))/(1.0+test(20)*exp(((i-1)*0.1)**2/2.0))
        if (iprn .ge. 3) then
          write(punt,610) i,wf(i)
610       format(30x,i5,f12.3)
        endif
615   continue
      if (iprn .ge. 3) write(punt,595)
      if (ilis.gt.0) then
        write(punt, 620)  ( i,atest(i), test(i), inote(i), i =  1, 10)
        write(punt, 625)  ( i,atest(i), test(i), inote(i), i = 11, 20)
        write(punt, 630)  ( i,atest(i), test(i), inote(i), i = 21, 30)
        write(punt, 635)  ( i,atest(i), test(i), inote(i), i = 31, 40)
        write(punt, 640)  ( i,atest(i), test(i), inote(i), i = 41, 50)
        write(punt, 642)  ( i,atest(i), test(i), inote(i), i = 51, 55)
      endif
620   format(/7x, 'test variables', 14x, 'description', /, 5x,
     *'standard  reset to',
     */i3,2f10.4,a4,'ratio of p-wave velocity to s-wave velocity.',
     */i3,2f10.4,a4,'lt 0 no elev cor/ =0 use 1st vel/ gt 0 use this.',
     */i3,2f10.4,a4,'first trial latitude in degrees.',
     */i3,2f10.4,a4,'first trial longitude in degrees.',
     */i3,2f10.4,a4,'first trial depth in kilometers, unless = -99.',
     */i3,2f10.4,a4,'sphere rad for aux rms values. if neg cont iterat',
     *'ion at most neg point.',
     */i3,2f10.4,a4,'minimum number of first motions required to plot.',
     */i3,2f10.4,a4,'elevation of top of layered models (km).',
     */i3,2f10.4,a4,'if 0 allow neg depths in summary and archive ',
     * 'files.',
     */i3,2f10.4,a4,'apply distance weighting on this iteration.')
625   format(
     *i3,2f10.4,a4,'xnear = greatest distance with weight of 1.0',
     */i3,2f10.4,a4,'xfar = least distnace with weight of 0.0',
     */i3,2f10.4,a4,'apply azimuthal weighting on this iteration.',
     */i3,2f10.4,a4,'weight out large residuals on this iteration.',
     */i3,2f10.4,a4,'give zero weight to residuals gt this.',
     */i3,2f10.4,a4,'apply boxcar weighting on this iteration.',
     */i3,2f10.4,a4,'give zero weight to residuals gt this*stand. dev.',
     */i3,2f10.4,a4,'begin jeffreys weighting on this iteration.',
     */i3,2f10.4,a4,'use jeffreys weighting only if rms gt this.',
     */i3,2f10.4,a4,'mu of jeffreys weighting funct.')
630   format(
     * i3,2f10.4,a4,'maximum number of iterations.',
     */i3,2f10.4,a4,'limit change in focal depth to this amount (km).',
     */i3,2f10.4,a4,'if delz would make z neg, set delz = -this*z ',
     *'(km).',
     */i3,2f10.4,a4,'limit change in epicenter to this. (km).',
     */i3,2f10.4,a4,'fix depth if epicentral change gt this. (km).',
     */i3,2f10.4,a4,'stop iterating if square of adjustment lt this.',
     */i3,2f10.4,a4,'global opt: if deep solution z > this, continue',
     *' with z 1/2 way to surface.',
     */i3,2f10.4,a4,'for fixed hypo on plane, set = plunge azimuth.',
     *'  if neg. continue as free sol.',
     */i3,2f10.4,a4,'set std err of res=+this if degrees of freedom =',
     *'0 or =-this if this lt 0.'
     */i3,2f10.4,a4,'dip of plunge vector for epi. fixed on plane. ',
     *' see test(28) & (47) also.')
635   format(
     * i3,2f10.4,a4,'duration magnitude c1, constant.',
     */i3,2f10.4,a4,'duration magnitude c2, *log((f - p)*fmgc).',
     */i3,2f10.4,a4,'duration magnitude c3, *delta.',
     */i3,2f10.4,a4,'if not 0, scale the normal equations.',
     */i3,2f10.4,a4,'minimum damping of normal equations.  ',
     */i3,2f10.4,a4,'maximum first trial depth if computed from p-',
     *'arrival times.',
     */i3,2f10.4,a4,'if termination occurs before this iteration, set ',
     *'iteration number to this and continue.',
     */i3,2f10.4,a4,'if this =1, run all with and then without s/ =2,',
     *'run with s/ =3, run without s/ =4, fix hypo', /,
     * 27x, '   / neg, use s to fix origin.',
     */i3,2f10.4,a4,'multiply the s and s-p weights by this factor.',
     */i3,2f10.4,a4,'duration magnitude c4, *depth.')
640   format(
     * i3,2f10.4,a4,'if this =1, print opt. ge 1, & summary ',
     *'opt. =+ or -1, then write sum. record each itteration.',
     */i3,2f10.4,a4,'global opt: deep starting z wrt top of model.',
     */i3,2f10.4,a4,'duration magnitude c5, *(log((f - p)*fmgc)**2).',
     */i3,2f10.4,a4,'if =1 rerun debug eqs with critical sta/ =2 ',
     *'continue iter with crit sta.',
     */i3,2f10.4,a4,'x scale factor for focal mechanism plot.',
     */i3,2f10.4,a4,'xfar set ge dist of test(46)th station + 10.  if ',
     *'lt 0 then fill gap.',
     */i3,2f10.4,a4,'weight for fix on plane.  see test(28) and (30).',
     */i3,2f10.4,a4,'half-space velocity for first trial location.',
     */i3,2f10.4,a4,'if .ne. 0 calculate vp/vs ratio; if abs val >1 ma',
     *'ke wadati plot; if neg, use wadati origin in solution.',
     */i3,2f10.4,a4,'for exploring rms space, compute this number of',
     *' fixed depth solutions (up to 22).')
642   format(
     */i3,2f10.4,a4,'for epicentral distance beyond this, use first',
     *' travel-time table.',
     */i3,2f10.4,a4,'Wood Anderson static magnification assumed for',
     *' local magnitude determination.',
     */i3,2f10.4,a4,'if .eq. 1 stations with 4-letter codes ending',
     *' e or n treated as horizontals.',
     */i3,2f10.4,a4,'if 1st computed trial location > this (km) from',
     *' closest station, start at closest station.',
     */i3,2f10.4,a4,'assumed century for events without summary',
     *' record.')

      j = iq
      if (ioldq .eq. 1) j = -iq
      if (ilis.gt.0) then
        write(punt, 645) rsew
645     format(/,
     *  ' weight option - relative standard errors for code:   0'
     *, '      1      2      3', /,
     *         '                                                   ',
     *    4f7.3, /)
        write(punt,650) iprn,ipun,imagsv,j
650     format(' printer option ', i4, 2x, ' summary option  ', i4, 5x,
     * ' magnitude option ', i4, 5x, ' tabulation option ',i2,//
     * ' no event output  -2', /,
     * ' one line/eq      -1', /,
     * ' final solution    0   no sum records     0      use xmag',
     * 12x, '0      no summary        0', /
     * ' one line per iter 1   summary records    1      use fmag',
     * 12x, '1      a                 1', /
     * ' sta res each iter 2   sum + archive file 2      use (xmag+fm'
     *,'ag)/2   2      a + b             2', /
     * 70h regres each iter  3   archive file       3      prefer fmag /
     *xmag   3,
     * 6x, 19ha,b + c           3,/
     * '                       "corrected" input  4  ',
     * '    prefer xmag /fmag   4      a,b,c + d         4',
     * /, 23x, '                          if neg use fms not fmp',
     *  5x, 'positive/q from std errors', /
     * 45x, 31x, 'negative/q from sol+sta')
        if(uacal .ne. ' ') write(punt, 654) uacal
654     format(' u of a cal data file:  ', a)
      endif
      if (nedit .ne. 0 .and. ilis .gt. 0)
     * write(punt,655) nedit,rmsmx,presmx,sresmx,
     *                              noutmx,nimx,semx
655   format(' debug code = ',i1,'.    debug events have:',
     * /,'                                           rms .gt. ',f5.2,
     * /,'                                 or a p res is .gt. ',f5.2,
     * /,'                                or an s res is .gt. ',f5.2,
     * /,'        or the no. of readings weighted out is .gt. ',i5,
     * /,'                or the number of iterations is .gt. ',i5,
     * /,'  or the max single variable standard error is .gt. ',f5.0,
     * /,'                     or the change in depth is .lt. ',f5.2)
c----
      if (nkrst .eq. 0) then
        if (kl .eq. 0) then
          write(punt, '(a,/,a)') 
     *    ' xxx error xxx:  ',
     *    ' at least one layered crustal model must be specified.'
          stop 'abort from input1'
        endif
        goto 790
      endif
c scan and print velocity structure data read in above
      kl = 1
      lbeg(1) = 1
      if (test(1) .le. 0.0) then
        write(logfil, 660) test(1)
        write(punt, 660) test(1)
660     format(' xxx error xxx', /,
     *  ' test(1) is the vp/vs ratio and may not equal ', f10.2)
        stop 'abort from input1'
      endif
      if (ivlr.ne.0.and.ilis.gt.0) write(punt,665) ivlr,ivway
665   format(54h variable layer option is used.  layer to be varied is
     *,7h layer ,i3,9h    vmod=,i3)
      if (lowv .eq. 0.and.ilis.gt.0) write(punt,670)
670   format(47h do not make compensating change in layer below,
     * 16h variable layer.)
      if (lowv .eq. 1.and.ilis.gt.0) write(punt,671)
671   format(' make compensating change in layer below',
     * ' variable layer.')
      if (iprun .ne. 1.and.ilis.gt.0) write(punt,680)
680   format(' residual option is in effect.  for residuals between + '
     *  ,49hand - 2.25 only the absoulte value rounded to the,
     * /,41h      nearest 0.5 second will be printed.)
      if (ilis.gt.0) write(punt,685) kl
685   format(//7x,14hvelocity model ,i3
     */29h   layer  velocity     depth ,
     * 21h  thickness    vpvs  /11x,25hkm/sec       km        km/)
      lp1 = lmax + 1
      do 690 l = 1, nkrst
690   if (vpvs(l) .eq. 0.0) vpvs(l) = test(1)
      do 785 l = 1,lp1
        thk(l) = d(l+1)-d(l)
        if (thk(l).lt.0.0) thk(l) = 1000.000
        if ( (d(l) .ne. 0.) .or. (l .eq. 1) ) goto 770
c found depth of zero, so begin new model
        lend(kl) = l-1
        if (lend(kl) .ne. lbeg(kl)) goto 700
        write(punt,695)
        write(logfil,695)
695     format(//' xxxerrorxxx half space model not allowed.'/
     *         ' so stop.')
        stop 'abort from input1'
c check for variation in vp/vs
700     vpvsm(kl) = vpvs(lbeg(kl))
c        print *, 'vpvs for model ', kl, ' is ', vpvsm(kl)
c        print *, 'lbeg = ', lbeg
c        print *, 'lend = ', lend
c        print *, 'vpvs = ', vpvs
        do 705 i = lbeg(kl) + 1,lend(kl)
          if (vpvsm(kl) .ne. vpvs(i)) goto 710
705     continue
        goto 745
c define s model
c need to define s model
710     vpvsm(kl) = 0.0
        ishft = lend(kl) + 1 - lbeg(kl)
        nkrstn = nkrst + ishft
        if (nkrstn .le. lmax) goto 720
        write(punt,715) nkrstn,lmax
        write(logfil,715) nkrstn,lmax
715     format(//' xxxerrorxxx ',i5,' exceeds',i5,' the max. number'
     *  ,' of layers.',/,' so stop.')
        stop 'abort from input1'
720     ntomv = nkrst - lend(kl)
        nkrst = nkrstn
        if (ntomv .eq. 0) goto 730
c shift down remaining models
        do 725 i = 1,ntomv
          ii = lend(kl) + ntomv + 1 - i
          iii = nkrstn + 1 - i
          v(iii) = v(ii)
          d(iii) = d(ii)
          vpvs(iii) = vpvs(ii)
725     continue
730     do 735 i = 1,ishft
          ii = lbeg(kl) - 1 + i
          iii = lend(kl) + i
          v(iii) = v(ii)/vpvs(ii)
          d(iii) = d(ii)
          vpvs(iii) = 0.0
735     continue
        if (ilis.gt.0) write(punt,740)
740     format(//' the next model is for s only:')
        mtyp(kl) = 'p '
        mtyp(kl+1) = 's '
745     if (v(l) .eq. -1.) goto 790
        if (v(l).lt.0.01) then
          write(logfil, 750) v(l), kl
          write(punt, 750) v(l), kl
750       format(' xxxerrorxxx velocity may not be less than .01',
     *    ' but was ', f10.4, ' in model ', i4, ', so stop!')
          stop 'abort from input1'
        endif
        kl = kl+1
        if (kl.le.mmax) goto 760
        write(punt,755) mmax
        write(logfil,755) mmax
755     format(//' xxxerrorxxx too many velocity models. only ',i3,
     *  ' models allowed. so stop.')
        stop 'abort from input1'
760     lbeg(kl) = l
        if (ilis.gt.0) write(punt,685) kl
        thk(l) = d(l+1) - d(l)
        if (d(l) .eq. 0.0) goto 770
        write(punt,765)
        write(logfil,765)
765     format(//' xxxerrorxxx each structure must begin at a',
     *         ' depth of 0.0 km.  so stop.')
        stop 'abort from input1'
770     vi(l) = 1/v(l)
        vsq(l) = v(l)**2
        if (ilis.gt.0) write(punt,775) l,v(l),d(l),thk(l),vpvs(l)
775     format(1x,i5,2x,4f10.3)
        if ((l-lbeg(kl)+1).le.lmmax) goto 785
        write(punt,780) kl, lmmax
        write(logfil,780) kl, lmmax
780     format(//' xxxerrorxxx velocity model',i3,' has too many ',
     *         'layers.  only',i3,' layers are allowed.',/,
     *         ' so stop.')
        stop 'abort from input1'
785   continue
790   continue
c     take care of travel time tables.
      if( nttab .ne. 0) then
	do 792 i = 1, nttab
          call hycrt(i, i+20, vpvsm(i+mmax) ) 
	  if ( (vpvsm(i+mmax) .lt. 0.) .and. (nttab .lt. i+1) ) then
            write(logfil, '(3a)')
     *        ' negative vp/vs in third travel-time table requires',
     *        ' that a forth table be include for s travel times.',
     *        '  However, program is currently limited to 3 tables.'
            stop 'abort from input1'
	  endif
          modin(i+mmax) = i
          if (vpvsm(i+mmax) .eq. 0.0) vpvsm(i+mmax) = test(1) 
	  if (test(1) .lt. 0.) then
	    write(logfil, '(a)') 
     *        ' test(1) (vp/vs ratio) may not be negative'
            stop 'abort from input1'
	  endif
	  if ((i .gt. 1) .and. 
     *      (vpvsm(i+mmax-1) .lt. 0.)) modin(i+mmax) = 0
792	continue
      endif

      do 810 l = 1, ns
        if (mod(l) .le. kl) then
c         this is fine, unless it is an s model
          if ( mtyp(mod(l)) .eq. 's ') then
            write(punt, 795) nsta(l), mod(l)
            write(logfil, 795) nsta(l), mod(l)
795         format(' xxxerrorxxx velocity model specified for station ',
     *      a, ' was ', i4, ', an s-model, so stop!')
            stop 'abort from input1'
          endif
        else if ( ((mod(l) .gt. kl) .and. (mod(l) .lt. 11)) .or.
     *            (mod(l) .gt. mmax+3) ) then
c         model out of range, so switch to highest model
          msav = mod(l)
          mod(l) = kl
c         if the last model is an s model, use the previous one.
          if (mtyp(kl) .eq. 's ') mod(l) = kl - 1
          write(punt, 800) nsta(l), msav, mod(l)
          write(logfil, 800) nsta(l), msav, mod(l)
800       format(' xxx warning xxx velocity model specified for station',
     *    a, ' was too large (equaled ',i3, ').  reset to ', i3/)
        else if ( modin(mod(l)) .eq. 0 ) then
c         mod(l) is mmax+1, +2, or +3, and it doesn't exist or is not a p model
          write(punt, 801) nsta(l), mod(l)
          write(logfil, 801) nsta(l), mod(l)
801	  format(' xxx error xxx velocity model specified for station',
     *      a, '(', i3, 
     *      ') was not defined or was assigned to an s table.')
          stop 'abort from input1'
	endif
810   continue
      if (nkrst .eq. 0) goto 871
c set up arrays to be used in trvdrv
      do 815 l = 1, lmax
815   jref(l) = 0
      if (iprn .le. 3) goto 825
      if (ilis.gt.0) write(punt, 820)
820   format(//1x, '   leq    m      tid(leq, m)      did(leq, m) ' ,
     * 38x, 'input1 formats 700 and 760.')
825   do 870 imod = 1, kl
c       loop through models, imod
        nlayers = lend(imod) - lbeg(imod)+1
        do 840 leq = lbeg(imod), lend(imod)
c         loop through eq layers, leq
          do 839 mrefr = 1, nlayers
            mref = lbeg(imod) - 1 + mrefr
            if ( (mref .gt. leq) .and. (v(mref) .gt. v(leq)) ) then
              vsqd(mrefr, leq) =
     *          sqrt((v(mref)/v(leq)-1.)*(v(mref)/v(leq)+1.))
            else
              vsqd(mrefr, leq) = 0.
            endif
            if (leq .ge. mref) then
              f(leq, mrefr) = 2.0
            else
              f(leq, mrefr) = 1.0
            endif
839       continue
840     continue
        ivl = lbeg(imod) + ivlr - 1
        do 870 leq = lbeg(imod), lend(imod)
c         loop through eq layers, leq
          leqr = leq - lbeg(imod) + 1
          if (leq .eq. ivl) then
c           preserve original thicknesses of variable layers
            sthk(imod) = thk(leq)
            sthk1(imod) = thk(leq + 1)
          endif
          do 870 mrefr = leqr, nlayers
c           loop through refracter layers, mrefr
            if (mrefr .eq. 1) goto 870
            sumt = 0.0
            sumd = 0.0
            do 850 l = lbeg(imod), lbeg(imod) + mrefr - 2
c             true if mref .le. l .or. v(mref) .le. v(l)
c             skip if refracter velocity .le. any overlaying velocity
c             if (vsqd(mrefr, l) .eq. 0.) goto 860
              if (v(lbeg(imod) + mrefr - 1) .le. v(l)) goto 860
850         continue
            jref(mrefr + lbeg(imod) - 1) = 1
            do 855 l = lbeg(imod), lbeg(imod) + mrefr - 2
c             loop from top layer to layer above refractor
              if (((l.eq.ivl) .or. (l.eq.ivl+1))
     *        .and. (ivlr .gt. 0))goto 855
              sumt = sumt + f(l, leqr)*thk(l)*vsqd(mrefr, l)
              sumd = sumd + f(l, leqr)*thk(l)/vsqd(mrefr, l)
855         continue
860         tid(leq, mrefr) = sumt*vi(mrefr + lbeg(imod) - 1)
            did(leq, mrefr) = sumd
            if (iprn .le. 3) goto 870
            if (ilis.gt.0) write(punt, 865)
     *        leq, mrefr, tid(leq, mrefr), did(leq, mrefr)
865         format(1x, 2i5, 2f15.3)
870   continue
871   do 875 l = 1, ns
875   thks(l) = blank
c set up trial hypocenter read in as test variables
      latr = 0.0
      lonr = 0.0
      if ((abs(test(3))+abs(test(4))).le.0.00001) goto 880
      la = abs(test(3))
      lo = abs(test(4))
      ala = (abs(test(3)) - la)*60.
      alo = (abs(test(4)) - lo)*60.
      ins = 'n'
      iew = 'w'
      if (test(3) .lt. 0.) ins = 's'
      if (test(4) .lt. 0.) iew = 'e'
      call fold2(latr, lonr, la, ins, ala, lo, iew, alo)
880   continue
885   continue
      return
      end
c end input1
c inside.for    []
      integer function inside(x0, y0, px, py, n)
c     check if point x0, y0 is inside polygon px(i), p(y), i = 1 to n
c     (n is the number of vertaces - eg. for a rectangle, n = 4)
c     based on godkin & pulli, bssa 74, p. 1845-1848, 1984.
c     converted to fortran from b. julian's c implementation
c     by j. c. lahr  -  18 may 1986
c     returns 0 if point outside polygon
c             1 if point at vertex
c           +-4 if point on an edge
c           +-2 if point inside
c
      logical vertex
      dimension px(n), py(n)
      inside = 0
      vertex = .false.
      x1 = px(n) - x0
      y1 = py(n) - y0
      do 20 i = 1, n
c       print *, '******************>> line segment ', i, vertex
        x2 = px(i) - x0
        y2 = py(i) - y0
        y1y2 = y1*y2
        if(y1y2 .le. 0.) then
c         print *, ' line crosses or at least touches x axis'
          ksi =  ksicr(x1, y1, x2, y2, y1y2, vertex)
c         print *, 'ksi = ', ksi
          if(iabs(ksi) .eq. 4) then
            inside = ksi
            return
          endif
          if(vertex) then
            inside = 1
            return
          endif
          inside = inside + ksi
        endif
        x1 = x2
        y1 = y2
20    continue
      return
      end
c end inside
c iprst.for    []
      subroutine iprst(l,llat,ins,alat,llon,iew,alon)
c     print out station data
      include 'params.inc' 
      parameter (ndly = 11)
      character*1 ins, iew
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn), xmwt(nsn)
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
c
      if (ilis .gt. 0)
     * write(punt,100) nsta(l), llat, ins,   alat, llon, iew,    alon,
     *  ielv(l), ipthk(l), vthk(1,l), vthk(2,l), mod(l), ipdly(l),
     * (dly(i,l), sdly(i,l),i=1,5), klas(1, l), calr(1, l), xmgc(l),
     * xmwt(l), fmwt(l), fmgc(l), ipcod(l), iscod(l), (revp(i,l),i=1,6),
     * sw(l), tpdly(1,l), (exdly(ii, l), ii = 1, 4),
     * tpdly(2,l), ndate(l), nhr(l)
  100 format(        1x,a5, 1x,i2,  a1,1x,f5.2,1x,i3,   a1, 1x,f5.2,
     *      i5,        i2,             1x,2f4.2,     i3,       i2,
     * 1x,             5(2f5.2,1x),        i2,    1x,f5.2,  1x,f4.2,
     *      i2,      i2, 1x,f4.2,             1x,2i2,      /,' * ', 6a1,
     *  3x,f4.1, 1x, f5.2,                      1x,4a1,
     *   1x,f5.2,   1x, i8,  1x, i2)
      return
      end
c end iprst
c ksicr.for    []
      integer function ksicr(x1, y1, x2, y2, y1y2, vertex)
c     signed crossing number - converted to fortran by j. c. lahr from
c       b. julian's c code.    18 may 1986
c
c       0                          :  does not cross -x axis
c       0 with vertex set to true  :  one end at point
c       +/-4                       :  goes through point
c       +/-2                       :  crosses -x axis
c       +/-1                       :  half crosses -x axis
c     [test for y1*y2 .le. 0. is performed in calling routine.]
c     [otherwise y1*y2 .gt. 0. would be placed here and return zero.]
      logical vertex
      t = x1*y2 - x2*y1
c     print *, 't = ', t
      if( (t .eq. 0.) .and. (x1*x2 .le. 0.) ) then
c       print *, 'line passes through or to point'
        if(y1y2 .ne. 0.) then
c         print *, 'neither end of line terminates at point'
          if(y2 .gt. 0.) then
c           print *, 'line passes up through point'
            ksicr = 4
          else
c           print *, 'line passes down through point'
            ksicr = -4
          endif
          return
        else if(x1*x2 .eq. 0.) then
c         print *, 'y1*y2 = 0. and x1*x2 = 0.'
          vertex = .true.
c         print *, 'vertex = ', vertex
        else
          if(x1 .lt. x2) then
c           print *, 'line passes to right through point'
            ksicr = 4
          else
c           print *, 'line passes to left through point'
            ksicr = -4
          endif
          return
        endif
      else if(y1y2 .lt. 0.) then
c       print *, 'complete crossing of x axis'
        if(t*y2 .lt. 0) then
c         print *, 'complete crossng of -x asix'
          if(y2 .gt. 0.) then
            ksicr = 2
          else
            ksicr = -2
          endif
          return
        endif
      else if(y1 .eq. 0.) then
c       print *, 'half crossing, y1 equals 0'
        if((x1 .lt. 0) .and. (y2 .ne. 0.) ) then
          if(y2 .gt. 0.) then
            ksicr = 1
          else
            ksicr = -1
          endif
          return
        endif
      else
c       print *, 'half crossing, y2 must equal 0'
        if(x2 .lt. 0) then
          if(y1 .lt. 0) then
            ksicr = 1
          else
            ksicr = -1
          endif
          return
        endif
      endif
      ksicr = 0
      return
      end
c end ksicr
c lentru.for    []
      integer function lentru(alph)
c     finds the true length of a character variable
      character alph*(*)
      l = len(alph)
      do 100 i = l, 1, -1
       if(alph(i:i).ne.' ' .and. alph(i:i).ne.'\0') then
         lentru = i
         return
        endif
100   continue
      lentru = 0
      return
      end
c end lentru
c line3.for    []
      subroutine line3(nr, p, s, pwt, swt, test,
     *                         vpvs1, sint1, orig1, se1, ses, oses,
     *                         vpvs2, sint2, orig2, se2, sep, osep,
     *                         vpvs3, sint3, orig3, se3)
c bi-weighted regression:
c assume p and s both have errors.  regress s on p.  (madansky, 1959)
c
      include 'params.inc' 
c calling arguments:
      integer           nr
c                                 ! number of arrival time pairs
      real              p(nr)
c                                 ! p arrival times
      real              s(nr)
c                                 ! s arrival times
      real              pwt(nr)
c                                 ! p weights
      real              swt(nr)
c                                 ! s weights
      real              vpvs1
c                                 ! [output] vpvs ratio   (s vs p)
      real              vpvs2
c                                 ! [output] vpvs ratio   (p vs s)
      real              vpvs3
c                                 ! [output] vpvs ratio   (bi-weight)
      real              sint1
c                                 ! [output] s intercept  (s vs p)
      real              sint2
c                                 ! [output] s intercept  (p vs s)
      real              sint3
c                                 ! [output] s intercept  (bi-weight)
      real              orig1
c                                 ! [output] orig time  (s vs p)
      real              orig2
c                                 ! [output] orig time  (p vs s)
      real              orig3
c                                 ! [output] orig time  (bi-weight)
      real              oses
c                                 ! rms difference between s obs & comp
      real              osep
c                                 ! rms difference between p obs & comp
      real              se1
c                                 ! [output]standard error (s vs p)
      real              se2
c                                 ! [output]standard error (p vs s)
      real              se3
c                                 ! [output]standard error (bi-weight)
c
c                                 ! sep & ses are not allowd to go below test(29
      real              sep
c                                 ! [output]s.e. of p used in s.e. of vp/vs (p v
      real              ses
c                                 ! [output]s.e. of s used in s.e. of vp/vs (s v
      real              test(50)
c                                 ! test parameters
c
      double precision  dubl
c                                 ! statement function
      double precision  arg
c                                 ! variance
      double precision  dslop1
c                                 ! vpvs ratio (s vs p)
      double precision  dslop2
c                                 ! vpvs ratio (p vs s)
      double precision  eqwt(npa)
c                                 ! equation weight
      double precision  dorig1
c                                 ! origin (s vs p)
      double precision  dorig2
c                                 ! origin (p vs s)
      double precision  dorig3
c                                 ! origin (bi-weight)
      double precision  dp(npa)
c                                 ! p
      double precision  ds(npa)
c                                 ! s
      double precision  dse1
c                                 ! se (s vs p)
      double precision  dse2
c                                 ! se (p vs s)
      double precision  dse3
c                                 ! se (bi-weight)
      double precision  dsint1
c                                 ! sinter (s vs p)
      double precision  dsint2
c                                 ! sinter (p vs s)
      double precision  dsint3
c                                 ! sinter (bi-weight)
      double precision  dvpvs
c                                 ! increment in vpvs
      double precision  pav
c                                 ! bi-weighted average of p
      double precision  pavp
c                                 ! average value of p weighted with p weights
      double precision  pavs
c                                 ! average value of p weighted with s weights
      double precision  pssum
c                                 ! sum weighted p*s
      double precision  psum
c                                 ! sum p times
      double precision  p2sum
c                                 ! sum weighted p**2
      double precision  sav
c                                 ! bi-weighted average of s
      double precision  savp
c                                 ! average value of s weighted with p weights
      double precision  savs
c                                 ! average value of s weighted with s weights
      double precision  ssum
c                                 ! sum s times
      double precision  s2sum
c                                 ! sum weighted s**2
      double precision  tsum
c                                 ! trial sum to be minimized
      double precision  tsumin
c                                 ! minimum sum
      double precision  vpvsp
c                                 ! current preferred value of vpvs
      double precision  vpvsp1
c                                 ! next preferred value of vpvs
      double precision  vpvst
c                                 ! trial lower limit of vpvs
      double precision  wpsum
c                                 ! bi-weighted sum of p
      double precision  wpsump
c                                 ! sum of p times weighted with p weights
      double precision  wpsums
c                                 ! sum of p times weighted with s weights
      double precision  wssum
c                                 ! bi-weighed sum of s
      double precision  wssump
c                                 ! sum of s times weighted with p weights
      double precision  wssums
c                                 ! sum of s times weighted with s weights
      double precision  wtsum
c                                 ! sum of bi-weights
      double precision  wtsump
c                                 ! sum of p weights
      double precision  wtsums
c                                 ! sum of s weights
c
c initialize some variables
      psum = 0.d0
      ssum = 0.d0
      wpsum = 0.d0
      wpsump = 0.d0
      wpsums = 0.d0
      wssump = 0.d0
      wssums = 0.d0
      wssum = 0.d0
      p2sum = 0.d0
      s2sum = 0.d0
      pssum = 0.d0
      wtsum = 0.d0
      wtsump = 0.d0
      wtsums = 0.d0
c
c main loop to compute vp/vs and origin time
      do 40 i=1,nr
        pi = p(i)
        si = s(i)
        dp(i) = dubl(pi)
        ds(i) = dubl(si)
        wpsump = wpsump + pwt(i)*dp(i)
        wpsums = wpsums + swt(i)*dp(i)
        wtsump = wtsump + pwt(i)
        wssump = wssump + pwt(i)*ds(i)
        wssums = wssums + swt(i)*ds(i)
        wtsums = wtsums + swt(i)
40    continue
c compute weighted and unweighted averages
      pavp = wpsump/wtsump
      pavs = wpsums/wtsums
      savp = wssump/wtsump
      savs = wssums/wtsums
c
c compute slop (vp/vs) based on s dependent on p
      do 50 i=1,nr
        p2sum = p2sum + swt(i)*(dp(i)-pavs)**2
        pssum = pssum + swt(i)*(dp(i)-pavs)*(ds(i)-savs)
        s2sum = s2sum + swt(i)*(ds(i)-savs)**2
50    continue
cd    print *, 's2sum, savs, pssum, pavs, p2sum'
cd    print *,  s2sum, savs, pssum, pavs, p2sum
c
      dslop1 = pssum/p2sum
c compute s.e. of s readings with weight code of zero (ses)
      arg = ( s2sum - dslop1*pssum ) / (nr - 2.d0)
      if (arg .lt. 0.d0) then
        print *, 'probable round off error, arg = ', arg
        print *, ' but arg should not be less than 0.'
        arg = 0.d0
      endif
      oses = dsqrt(arg)
c use the larger estimate of ses
      ses = oses
      if (ses .lt. abs(test(29))) ses = abs(test(29))
      dse1 = ses/dsqrt(p2sum)
cd    print *, 'ses, test(29) ', ses, test(29)
c
      dsint1 = savs - dslop1*pavs
      if(dslop1 .eq. 1.0) dslop1 = 1.01
      dorig1 = (dslop1*pavs-savs)/(dslop1-1.0)
cd     write(7, 51) dslop1, dse1
cd51    format(' for s vs p regression, vpvs = ', d10.4, ' and se = ',
cd   *   d10.4)
c
c compute slope (vp/vs) based on p dependent on s
      p2sum = 0.d0
      pssum = 0.d0
      s2sum = 0.d0
      do 52 i=1,nr
        p2sum = p2sum + pwt(i)*(dp(i)-pavp)**2
        pssum = pssum + pwt(i)*(dp(i)-pavp)*(ds(i)-savp)
        s2sum = s2sum + pwt(i)*(ds(i)-savp)**2
52    continue
cd    print *, 's2sum, savp, pssum, pavp, p2sum'
cd    print *,  s2sum, savp, pssum, pavp, p2sum
c
      dslop2 = pssum/s2sum
c compute s.e. of p readings with weight code of zero (sep)
      arg = ( p2sum - dslop2*pssum ) / (nr - 2.d0)
      if (arg .lt. 0.d0) then
        print *, 'probable round off error, arg = ', arg
        print *, ' but arg should not be less than 0.'
        arg = 0.d0
      endif
      osep = dsqrt(arg)
      sep = osep
      if (sep .lt. abs(test(29))) sep = abs(test(29))
      dse2 = sep/( dsqrt(s2sum)*(dslop2**2.d0) )
      dslop2 = 1.d0/dslop2
      dsint2 = savp - dslop2*pavp
      if(dslop2 .eq. 1.0) dslop2 = 1.01
      dorig2 = (dslop2*pavp-savp)/(dslop2-1.0)
cd     write(7, 53) dslop2, dse2
cd53    format(' for p vs s regression, vpvs = ', d10.4, ' and se = ',
cd   *   d10.4)
c
c find minimum sum
      dvpvs = .6
c     initial central value
      vpvsp = (dslop1 + dslop2)/2.d0
      tsumin = 99999.
c the central value need only be computed the first time
      do 60 j = 1, 8
        do 54 i = 1, 5
          vpvst = vpvsp - dvpvs*(-3 + i)
          tsum = 0.d0
c         compute averages
          do 43 k = 1, nr
            eqwt(k) = 1./( 1./swt(k) + (vpvst**2.d0)/pwt(k) )
            wpsum = wpsum + eqwt(k)*dp(k)
            wssum = wssum + eqwt(k)*ds(k)
            wtsum = wtsum + eqwt(k)
43        continue
          pav = wpsum/wtsum
          sav = wssum/wtsum
          do 44 k = 1, nr
            tsum = tsum + eqwt(k)*
     *                ( ds(k)-sav - vpvst*(dp(k)-pav) )**2
44        continue
          if (tsum .lt. tsumin) then
            tsumin = tsum
            vpvsp1 = vpvst
          endif
cd        print *, vpvst, tsum, tsumin
54      continue
cd     write(injump, 55) vpvsp1, tsum
cd55    format(d12.4, 3x, d12.4)
      vpvsp = vpvsp1
      dvpvs = dvpvs*.4
60    continue
c
      dse3 = dsqrt(dse1**2.d0 + dse2**2.d0)
      dsint3 = sav - vpvsp1*pav
      if(vpvsp1 .eq. 1.0) vpvsp1 = 1.01
      dorig3 = (vpvsp1*pav-sav)/(vpvsp1-1.0)
c convert to real for output
      vpvs1 = dslop1
      sint1 = dsint1
      se1 = dse1
      orig1 = dorig1
c
      vpvs2 = dslop2
      sint2 = dsint2
      se2 = dse2
      orig2 = dorig2
c
      vpvs3 = vpvsp1
      sint3 = dsint3
      se3 = dse3
      orig3 = dorig3
c
      return
      end
c end line3
c linv.for    []
c test driver for subroutine linv
c      data elevmx/3.0/, ielest/2000/, x/10./, zeq/3.0/
c      data v/3.0/, ak/1.0/
c      print *, 'welcome to linv!  this program tests subroutine'
c      print *, 'linv, which computes travel times in a half space'
c      print *, 'with linearly increasing velocity.'
c
c20    elevmx = raskk(
c     *'maximum topographic elevation (reference elevation in km)',
c     *          elevmx)
c      ielest = iaskk(
c     *'station elevation above sea level', ielest)
c      stz = elevmx - ielest*.001
c      v = raskk('velocity at reference elevation (km/s)', v)
c      ak = raskk('velocity gradient (km/s per km)', ak)
c30    x = raskk('epicentral distance (km)', x)
c      zeq = raskk('depth of eq beneath reference elevation (km)', zeq)
c
c      call linv(x, zeq, v, ak, t, ain, dtdd, dtdh, stz)
c      vsta = v + ak*(elevmx - ielest/1000.)
c      zeff = zeq - (elevmx - ielest/1000.)
c      call linvjl(x, zeff, vsta, ak, t, ain, dtdd, dtdh, stz,
c    *  vst, veq)
c
c      print *, 'linvjl results to test linv when eq is deeper',
c     * ' than station:'
c      print *, 'travel time (s) = ', t
c      ain =  asin(c      if(ain .lt. 0.) ain = 180. + ain
c      ain = 180. - ain
c      print *, 'ain in degrees = ', ain
c      print *, 'partial of tt wrt dist and depth = ', dtdd, dtdh
c
c      if(x .gt. 0.) goto 20
c      stop
c      end
      subroutine linv(x, zeq, v, ak, t, ain, dtdd, dtdh, stz,
     *  vst, veq)
c compute travel time and partial derivatives for a linear increasing
c velocity model in a region of significant topography.
      real x
c          x      epicentral distance in km
      real zeq
c          zeq    depth in km of earthquake beneath reference elevation
      real v
c          v      velocity at reference elevation in km/sec
      real ak
c          ak     velocity gradient km/sec per km
      real t
c          t      computed travel time in sec
      real ain
c          ain    angle-of-incidence
      real dtdd
c          dtdd   partial derivative of tt wrt distance
      real dtdh
c          dtdh   partial derivative of tt wrt depth
      real stz
c          stz    depth in km of station beneath reference elevation
      real vst
c          vst    velocity at station
      real veq
c          veq    velocity at earthquake
      real dz
c          dz     depth of eq beneath station
      real dc
c          dc     distance in km from eq or station, whichever is
c                 at higher elevation, to center of raypath circle
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
c      print *, 'station elevation (m)    = ', ielest
c      print *, 'eq depth (km)            = ', zeq
c      print *, 'epicentral distance      = ', x
c      print *, 'velocity at ref elev and grad = ', v, ak
      vst = v + ak*stz
      veq = v + ak*zeq
      dz = (zeq - stz)
      dist = sqrt(dz*dz + x*x)
      if (dist .lt. .0001) then
c       distance less than 10 cm
        t = 0.0
        dtdd = 1./veq
        dtdh = 1./veq
        anin = 1.
        ain = 90.
        return
      endif
      p = ak*ak*(x*x + dz*dz)/(2.*vst*veq) + 1.
      t = coshi(p)/ak
      if (dz .lt. 0) then
c earthqauke is above station (ray is down-going)
c        print *, 'eq is above station, dz = ', dz
        if (x .eq. 0) then
          anin = 0.0
          ain = 0.0
        else
          dc = (x*x + 2.*abs(dz)*veq/ak + dz*dz)/(2.*x)
          anin = (veq/ak) / sqrt(dc*dc + (veq/ak)**2)
          ain = deg*asin(anin)
c          print *, 'anin = ', anin
        endif
      else
c station is above earthquake (ray may be up- or down-going)
c        print *, 'station is above eq, dz = ', dz
        alam = vst*(cosh(ak*t) - 1.)/ak
        if(ak .le. .0001) alam = 0.0
        dif = dz - alam
        anin = x/sqrt(dif*dif + x*x)
      endif
      dtdd = x*ak/( vst*(vst + ak*dz)*sqrt(p*p - 1.) )
      dtdh = (dz*ak/(vst*(vst+ak*dz)) -
     *         ak*ak*(x*x + dz*dz)/(2.*vst*((vst + ak*dz)**2.)))/
     *         ((p*p - 1.)**0.5)
      if(dtdh .gt. 0.0) then
c       upgoing
        ain = 180. - deg*asin(anin)
      else
c       downgoing
        ain = deg*asin(anin)
      endif
      return
      end
c end linv
c linvol.for    []
      subroutine linvol(delta, depthsv, stzsv, ak, voa, aha, vi, t,
     *  ain, dtdd, dtdh)
c model:  linearly increasing velocity over a halfspace
c bill gawthrop  (modified by j.c. lahr  3/6/91)
c     delta  epicentral distance (km)
c     depthsv eq depth (km)
c     stzsv   station depth (km)
c     ak     velocity gradient
c     voa    v at surface
c     aha    thickness of linear layer (depth to half space)
c     vi     velocity of half space
c     t      travel time
c     ain    angle of incidence
c     dtdd   partial derivative of tt wrt distance
c     dtdh   partial derivative of tt wrt depth
      integer punt
      common /punt/ punt 
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      logical flip
      dist = sqrt(delta**2 + (depthsv - stzsv)**2)
      if (dist .lt. .0001) then
c       distance less than 10 cm
        t = 0.0
        dtdd = 1./(voa + ak*depthsv)
        dtdh = 1./(voa + ak*depthsv)
        ain = 90.
        return
      endif
      if((stzsv .ge. aha - .0001) .and.
     *  (depthsv .ge. aha - .0001)) then
c both eq and station are within (or within 10 cm) of the
c constant velocity halfspace
c       write(punt, *) 'eq and station both in halfspace'
        t = dist/vi
        dtdd = delta/(dist*vi)
        dtdh = (depthsv - stzsv)/(dist*vi)
        if (stzsv .eq. depthsv) then
          ain = 90.0
        else
          if(stzsv .lt. depthsv) then
c           upward
            ain = 180 + deg*atan(delta/(stzsv - depthsv))
          else
c           downward
            ain = deg*atan(delta/(stzsv - depthsv))
          endif
        endif
        return
      endif
      if(depthsv .ge. stzsv) then
        flip = .false.
        deptha = depthsv
        stza = stzsv
      else
        flip = .true.
        deptha = stzsv
        stza = depthsv
      endif
c if near boundary, then move to boundary
      if(abs(deptha - aha) .le. .0001) deptha = aha
      if(abs(stza - aha) .le. .0001) stza = aha
c set vsource
      if(flip) then
        if(stza .le. aha) then
          vsource = voa + stza*ak
        else
          vsource = vi
        endif
      else
        if(deptha .le. aha) then
	  vsource = voa + deptha*ak
	else
	  vsource = vi
	endif
      endif
c create modified model with station at surface
      ah = aha - stza
      vo = voa + ak*stza
      depth = deptha - stza
      stz = 0.0
      if (depth .le. ah) then
c source: within the layer
c       write(punt, *) ' source: within the layer'
        vz = vo + depth*ak
c       compute time of direct wave
        call tatime(delta,depth,vo,ak,ah,tat,aina)
c       compute time of refracted wave
        call tbtime(delta,depth,vo,ak,ah,vi,tbt,ainb)
        if (tbt .lt. tat) then
c         refracted wave is faster
          t = tbt
          if(flip) then
c           downward ray
            anin = vsource*sin(ainb)/vz
            ain = asin(anin)
          else
c           downward ray
            anin = sin(ainb)
            ain = ainb
          endif
        else if (tat .eq. 900000.) then
c         neither direct nor refracted wave could be computed!
          print *,
     *      'neither direct nor refracted wave could be computed!'
          print *, 'so stop'
          stop '1: abort from linvol'
        else
c         direct wave is faster
          t = tat
          if(flip) then
c           downward ray
            anin = vsource*sin(aina)/vz
            ain = asin(anin)
          else
c           upward or downward ray
            anin = sin(aina)
            ain = aina
          endif
        endif
      else
c source: within the halfspace
c       write(punt, *) ' source: within the halfspace'
c	write(punt, *) 'delta,depth,vo,ak,ah,vi'
c	write(punt, *)  delta,depth,vo,ak,ah,vi 
        call tdtime(delta,depth,vo,ak,ah,vi,td,aina)
        if (td.eq.900000.) then
c         direct wave could not be computed!
          print *,
     *      'direct wave could not be computed, so stop!'
          stop '2: abort from linvol'
        else
c         direct wave from halfspace
          t = td
          if(flip) then
c           downward ray
            anin = vsource*sin(aina)/vi
            ain = asin(anin)
          else
c           upward ray
            anin = sin(aina)
            ain = aina
          endif
        endif
      endif
      dtdd = anin/vsource
      dtdh = -cos(ain)/vsource
      ain = deg*ain
      return
      end
c end linvol
c lisinc.for    []
      subroutine lisinc(qno, sumrms, rms, jav, iq, nrwt, avwt, nr,
     * kdx, wt, nrp, xmag, fmag, blank, avxm, avfm, nxm, nfm, sxm,
     * sxmsq, sfm, sfmsq, sw, ipcod, ww, ksmp, nsmpr, ssmpr, x,
     * ssmpq, ssmpw, nres, sr, srsq, srwt, nresp, srp, srpsq, srpwt,
     * iscod, nsres, ssr, ssrsq, ssrwt, nsresp, ssrp, ssrpsq, ssrpwt)
c increment summary after each earthquake
      save
      include 'params.inc' 
      dimension kdx(npa), wt(npa), xmag(npa), fmag(npa), sw(nsn)
      dimension ipcod(nsn), ww(npa), ksmp(npa), x(4,npa), nres(nsn)
      dimension iscod(nsn)
      dimension nxm(nsn), nfm(nsn), sr(nsn), srsq(nsn),
     * srwt(nsn),sxm(nsn),sxmsq(nsn),sfm(nsn),sfmsq(nsn),qno(4)
      dimension nresp(nsn), srp(nsn), srpsq(nsn), srpwt(nsn)
      dimension nsresp(nsn), ssrp(nsn), ssrpsq(nsn), ssrpwt(nsn)
      dimension nsres(nsn),ssr(nsn),ssrsq(nsn),ssrwt(nsn),nsmpr(nsn),
     * ssmpr(nsn),ssmpq(nsn),ssmpw(nsn)
c
      qno(jav) = qno(jav) + 1
      sumrms = sumrms + rms
      if(jav .gt. iq) return
c compute the sqrt(sum of weights squared) for pavlis average residual calc
      srswq = 0.0
      if(nrwt .gt. 4) srswq = avwt*(nrwt - 4)
c accumulate summary of travel time residuals --- include p, s, and s-p
      do 825 i = 1, nr
c ji is index to station list for phase number i
        ji = kdx(i)
        wtu = wt(i)
        if(i .le. nrp) then
          if((xmag(i) .ne. blank) .and. (avxm .ne. blank)) then
            dxmag = xmag(i)-avxm
            nxm(ji) = nxm(ji)+1
            sxm(ji) = sxm(ji)+dxmag
            sxmsq(ji) = sxmsq(ji)+dxmag**2
          endif
          if((fmag(i) .ne. blank) .and. (avfm .ne. blank)) then
            dfmag = fmag(i)-avfm
            nfm(ji) = nfm(ji)+1
            sfm(ji) = sfm(ji)+dfmag
            sfmsq(ji) = sfmsq(ji)+dfmag**2
          endif
          if( (sw(ji) .eq. 0.) .or. ((ipcod(ji) .gt. 3) .and.
     *    (ipcod(ji) .lt. 9)) ) wtu = ww(i)
          if(wtu .eq. 0.) goto 825
          if(ksmp(i) .eq. 0) then
c smp residual calculation
            nsmpr(ji) = nsmpr(ji) + 1
            ssmpr(ji) = ssmpr(ji) + x(4,i)*wtu
            ssmpq(ji) = ssmpq(ji) + x(4,i)**2*wtu
            ssmpw(ji) = ssmpw(ji) + wtu
            goto 825
          else
c p-residual calculation
            nres(ji) = nres(ji) + 1
            sr(ji) = sr(ji) + x(4,i)*wtu
            srsq(ji) = srsq(ji) + x(4,i)**2*wtu
            srwt(ji) = srwt(ji) + wtu
c pavlis p-residual calculation
            if(srswq .gt. 0.0) then
              wtu = wtu*srswq
              nresp(ji) = nresp(ji) + 1
              srp(ji) = srp(ji) + x(4,i)*wtu
              srpsq(ji) = srpsq(ji) + x(4,i)**2*wtu
              srpwt(ji) = srpwt(ji) + wtu
            endif
            goto 825
          endif
        else
c s-residual calculation
          if( (sw(ji) .eq. 0.) .or. ((iscod(ji) .gt. 3) .and.
     *    (iscod(ji) .lt. 9)) ) wtu = ww(i)
          if(wtu .eq. 0.0) goto 825
          nsres(ji) = nsres(ji) + 1
          ssr(ji) = ssr(ji) + x(4,i)*wtu
          ssrsq(ji) = ssrsq(ji) + x(4,i)**2*wtu
          ssrwt(ji) = ssrwt(ji) + wtu
c pavlis s-residual calculation
          if(srswq .gt. 0.0) then
            wtu = wtu*srswq
            nsresp(ji) = nsresp(ji) + 1
            ssrp(ji) = ssrp(ji) + x(4,i)*wtu
            ssrpsq(ji) = ssrpsq(ji) + x(4,i)**2*wtu
            ssrpwt(ji) = ssrpwt(ji) + wtu
          endif
        endif
  825 continue
      return
      end
c end lisinc
c lissum.for    []
      subroutine lissum(lisco)
c output summary of time and magnitude residuals
      save
      include 'params.inc' 
      parameter (ndly = 11)
      real lat,lon
      character fixsw*1, fixp*1, fixs*1, jfmt*4, line*80
      character*1 ins, iew
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dhil/ iq,ilat,kms,icat
      common /dmost/ ipun,ivlr,blank
      common /hl/ nwad, tslope, tsqsl
      character*1 iqcls
      common /il1/ iqcls
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /ilmpu/ ns
      common /iclmpq/ lat(nsn),lon(nsn)
      common /ilotv/ elvdly(npa)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /ilv/ c(nsn),e(nsn)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
      common /lc/ nres(nsn)
      common /ohbl/ jav
      common /olnx/ xmag(npa),fmag(npa),avxm,avfm,xmmed,fmmed
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pnl/ ww(npa)
      common /pnoqtx/ ldx(npa)
      common /qmost/ wt(npa),z
      common /reloc/ irelo, nreloc
      common /rq/ xmean(4),avwt,aveaj
      common /tmost/ x(4,npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      dimension nxm(nsn), nfm(nsn), sr(nsn), srsq(nsn),
     * srwt(nsn),sxm(nsn),sxmsq(nsn),sfm(nsn),sfmsq(nsn),qno(4)
      dimension nresp(nsn), srp(nsn), srpsq(nsn), srpwt(nsn)
      dimension nsresp(nsn), ssrp(nsn), ssrpsq(nsn), ssrpwt(nsn)
      dimension nsres(nsn),ssr(nsn),ssrsq(nsn),ssrwt(nsn),nsmpr(nsn),
     * ssmpr(nsn),ssmpq(nsn),ssmpw(nsn),qnoper(4)
      dimension avres(7),sdres(7)
      data avres/7*0./
      if (iprn .ge. 5) write(punt, '(a, i5)') ' begin lissum ', lisco
      logu = 6
c initialize arrays
      if(lisco .eq. 1) then
        nwad = 0
        tslope = 0.
        tsqsl = 0.
        sumrms = 0.0
        do 48 l=1,ns
          nres(l)=0
          nresp(l)=0
          nres(l)=0
          nxm(l)=0
          nfm(l)=0
          sr(l)=0.
          srp(l)=0.
          srsq(l)=0.
          srpsq(l)=0.
          srwt(l)=0.
          srpwt(l)=0.
          nsres(l) = 0
          nsresp(l) = 0
          ssr(l) = 0.0
          ssrp(l) = 0.0
          ssrsq(l) = 0.0
          ssrpsq(l) = 0.0
          ssrwt(l) = 0.0
          ssrpwt(l) = 0.0
          nsmpr(l) = 0
          ssmpr(l) = 0.0
          ssmpq(l) = 0.0
          ssmpw(l) = 0.0
          sxm(l)=0.
          sxmsq(l)=0.
          sfm(l)=0.
          sfmsq(l)=0.
   48   continue
        do 49 i=1,4
          qno(i)=0.
   49   continue
        return
      endif
c
    4 if(lisco .eq. 0) then
        call lisinc(qno, sumrms, rms, jav, iq, nrwt, avwt, nr,
     *  kdx, wt, nrp, xmag, fmag, blank, avxm, avfm, nxm, nfm, sxm,
     *  sxmsq, sfm, sfmsq, sw, ipcod, ww, ksmp, nsmpr, ssmpr, x,
     *  ssmpq, ssmpw, nres, sr, srsq, srwt, nresp, srp, srpsq, srpwt,
     *  iscod, nsres, ssr, ssrsq, ssrwt, nsresp, ssrp, ssrpsq, ssrpwt)
        return
      endif
c
c write summary of time and mag residuals (statements 0 to 70)
      rewind 2
      qsum=qno(1)+qno(2)+qno(3)+qno(4)
      if(qsum .eq. 0.) then
        write(logu, 3)
        if(punt .ne. logu) write(punt, 3)
    3   format(' there were no events located in this run.')
        return
      endif
      averms = 0.0
      if(qsum .ne. 0.0) averms = sumrms/qsum
      avpvs = 0.
      if(nwad .gt. 0) avpvs = tslope/nwad
      sdpvs = 0.
      if(nwad .gt. 1) then
        arg = tsqsl/(nwad-1) - tslope*tslope/(nwad*(nwad-1))
        if(arg .lt. 0.0) arg = 0.0
        sdpvs = sqrt(arg)
      endif
      write(logu,5) averms, avpvs, nwad, sdpvs, (qno(i),i=1,4), qsum
      if(punt .ne. logu)
     *write(punt,5) averms, avpvs, nwad, sdpvs, (qno(i),i=1,4), qsum
    5 format(' average rms of all events = ', f10.5, /,
     *       ' average vp/vs ratio = ', f10.2, ' for ', i4, ' events.',
     *       '  standard deviation of ratio = ', f10.2,
     * //, ' ***** class/     a     b     c     d total *****',
     * //, '      number/', 5f6.1)
      do 10 i=1,4
   10 qnoper(i)=100.*qno(i)/qsum
      write(logu,15)(qnoper(i),i=1,4)
      if(punt .ne. logu) write(punt,15)(qnoper(i),i=1,4)
   15 format(/2x,'percentage/',4f6.1)
      if(iqcls .eq. '0') return
      write(logu,20) iqcls
      if(punt .ne. logu) write(punt,20) iqcls
   20 format(/,'   include only class ',a1,' and better in the',
     *  ' following statistics.' /)
   71 write(logu,1020)
      if(punt .ne. logu) write(punt,1020)
 1020 format(
     *'          ----------- p residuals --------------',
     *'    ----------- s residuals -------------', /,

     *'           no event wting        event wting',
     *'         no event wting        event wting', /,

     *' station  n  wt  ave   sd     n   wt  ave   sd',
     *'      n  wt   ave  sd     n   wt   ave  sd    station')

      do 70 i=1,ns

        do 30 j=1,7
          avres(j)=0.
          sdres(j)=0.
   30   continue

        avwt1 = 0.
        avwt6 = 0.
        if(nres(i) .gt. 0) then
c compute average p residual
          avres(1)=sr(i)/srwt(i)
c compute average weight
          avwt1 = srwt(i)/nres(i)
          if(nres(i) .gt. 1) then
	    temp = srsq(i)/srwt(i) - avres(1)**2
	    if(temp .gt. 0.0) then
              sdres(1)=sqrt(temp)
	    else
	      sdres(1)=0.0
	    endif
          endif
          if(nresp(i) .gt. 0) then
            if(srpwt(i) .gt. 0.0) then
c compute average residual, weight, and std. deviation a la pavlis
c see pavlis and hokanson (1985) - jgr, v 90, p 12777-12789
              avres(6)=srp(i)/srpwt(i)
              avwt6 = srpwt(i)/nresp(i)
	      temp = srpsq(i)/srpwt(i) - avres(6)**2
	      if(temp .gt. 0.0) then
                sdres(6)=sqrt(temp)
	      else
	        sdres(6)=0.0
	      endif
            endif
          endif
        endif

        avwt2 = 0.
        avwt7 = 0.
        if(nsres(i) .gt. 0) then
          avwt2 = ssrwt(i) / nsres(i)
          avres(2) = ssr(i)/ssrwt(i)

          if(nsres(i) .gt. 1) then
	    temp = ssrsq(i)/ssrwt(i) - avres(2)**2
	    if(temp .gt. 0.0) then
              sdres(2) = sqrt(temp)
	    else
	      sdres(2) = 0.0
	    endif
          endif

          if(nsresp(i) .gt. 0) then
            if(ssrpwt(i) .gt. 0.0) then
c compute average residual, weight, and std. deviation a la pavlis
              avres(7) = ssrp(i)/ssrpwt(i)
              avwt7 = ssrpwt(i) / nsresp(i)
	      temp = ssrpsq(i)/ssrpwt(i) - avres(7)**2
	      if(temp .gt. 0.0) then
                sdres(7) = sqrt(temp)
	      else
	        sdres(7) = 0.0
	      endif
            endif
          endif
        endif

        ipt = nres(i) + nsres(i)
        if(ipt .eq. 0) goto 70

        fixsw = ' '
        if (sw(i) .eq. 0.) fixsw = 'w'
        fixp = ' '
        if ( (ipcod(i) .gt. 3) .and.
     *  (ipcod(i) .lt. 9) ) fixp = 'p'
        fixs = ' '
        if ( (iscod(i) .gt. 3) .and.
     *  (iscod(i) .lt. 9) ) fixs = 's'

        write(logu, 65) fixsw, nsta(i),
     *  nres(i), avwt1, avres(1), sdres(1),
     *  nresp(i), avwt6, avres(6), sdres(6), fixp,
     *  nsres(i), avwt2, avres(2), sdres(2),
     *  nsresp(i), avwt7, avres(7), sdres(7), fixs, nsta(i)

        if(punt .ne. logu) write(punt, 65) fixsw, nsta(i),
     *  nres(i), avwt1, avres(1), sdres(1),
     *  nresp(i), avwt6, avres(6), sdres(6), fixp,
     *  nsres(i), avwt2, avres(2), sdres(2),
     *  nsresp(i), avwt7, avres(7), sdres(7), fixs, nsta(i)

   65   format(1x, a1, 1x, a5,
     *  2(i4, f4.1, 2f6.3, i4, f5.1, 2f6.3, a1), 2x, a4)

      if(irelo .gt. 0) then
        if(nreloc .eq. irelo) then
c write out revised primary station record
          call unfold2(lat(i),lon(i),la,ins,ala,lo,iew,alo)
          write(line, 67) nsta(i), la, ins,  ala, lo, iew,  alo,
     *    ielv(i), mod(i), ipthk(i)
67        format(              a5, i2,  a1, f5.2, i4,  a1, f5.2,
     *         i5,     i2,       i1)
c is a5 correct?  Shouldn't it be a4????????????

          call riorbk(vthk(1, i), ivt, jfmt, 4, 2)
          write(line(31:34), jfmt) ivt
          call riorbk(vthk(2, i), ivt, jfmt, 4, 2)
          write(line(35:38), jfmt) ivt
          write(line(39:39), '(i1)') ipdly(i)
          call riorbk(dly(1, i), idly, jfmt, 4, 2)
          write(line(40:43), jfmt) idly
          call riorbk(sdly(1, i), idly, jfmt, 4, 2)
          write(line(44:47), jfmt) idly
          write(13, '(a)') line
        else
          dly(1, i) = dly(1, i) + avres(6)
          sdly(1, i) = sdly(1, i) + avres(7)
        endif
      endif
   70 continue

      write(logu,1021)
      if(punt .ne. logu) write(punt,1021)
 1021 format(/,
     *'           s-p residuals        x-mag res       f-mag res', /
     *' station  n  wt  ave   sd     n  ave   sd     n  ave   sd')

      do 75 i=1,ns

        do 73 j=1,7
          avres(j)=0.
          sdres(j)=0.
   73   continue

        if(nxm(i) .gt. 0) then
          avres(3)=sxm(i)/nxm(i)
          if(nxm(i) .gt. 1) then
	    temp = sxmsq(i)/nxm(i) - avres(3)**2
	    if(temp .gt. 0.0) then
              sdres(3)=sqrt(temp)
	    else
	      sdres(3) = 0.0
 	    endif
          endif
        endif

        if(nfm(i) .gt. 0) then
          avres(4)=sfm(i)/nfm(i)
          if(nfm(i) .gt. 1) then
	    temp = sfmsq(i)/nfm(i) - avres(4)**2
	    if(temp .gt. 0.0) then
              sdres(4)=sqrt(temp)
	    else
	      sdres(4) = 0.0
	    endif
          endif
        endif

        asmpw = 0.
        if(nsmpr(i) .gt. 0) then
          asmpw = ssmpw(i) / nsmpr(i)
          avres(5) = ssmpr(i)/ssmpw(i)
          if(nsmpr(i) .gt. 1) then
	    temp = ssmpq(i)/ssmpw(i) - avres(5)**2
	    if(temp .gt. 0.0) then
              sdres(5) = sqrt(temp)
	    else
	      sdres(5) = 0.0
	    endif
          endif
        endif

        ipt = nsmpr(i) + nxm(i) + nfm(i)
        if(ipt .eq. 0) goto 75

        fixsw = ' '
        if (sw(i) .eq. 0.) fixsw = 'w'

        write(logu, 74) fixsw, nsta(i),
     *  nsmpr(i), asmpw, avres(5), sdres(5),
     *  nxm(i), avres(3), sdres(3), nfm(i), avres(4), sdres(4)

        if(punt .ne. logu) write(punt, 74) fixsw, nsta(i),
     *  nsmpr(i), asmpw, avres(5), sdres(5),
     *  nxm(i), avres(3), sdres(3), nfm(i), avres(4), sdres(4)

   74   format(1x, a1, 1x, a5, 
     *  (i4, f4.1, 2f6.3), 2(i4, 2f6.3), 4x, a4)

   75 continue
      return
      end
c end lissum
c locate.for    []
      subroutine locate(iterm)
      save sockets
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      real bat2,bon2,lat,lon
      character*120 erout
      character*4 malaz, malla, malor, dnstrg
      logical good, eoff, supout, sockets, needsum
      character*1 kwr, iqdo
c     if instset .ne. ' ', then put this new value of inst on summary record
      character*1 instset
      integer punt
      common /punt/ punt
      common /hnu/ instset
      common /anox/ keyd(npa)
      common /bqz/ avrps,avuse
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /char/ iahead, msta(npa), nsta(nsn), icard
      character*4 ipro, ichec, evtype*1, evstat*1, root*256
      common /dbhip/ ipro, ichec, evtype, evstat, root
      common /dmost/ ipun,ivlr,blank
      common /dhin/ iglob, zup, zdn
      common /dhil/ iq,ilat,kms,icat
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /dhio/ nedit,good
      common /dio/ rmsmx,presmx,sresmx,noutmx,nimx,iprun,semx
      common /dph/ noswt, eoff
      common /gg/ altla(3), altlo(3), altz(3), altrms(3), nsolut,
     * fla(20), flo(20), fz(20), frms(20), forg(20), maxf, altorg(3)
      common /ghnq/ iexit
      common /gmost/ az(npa)
      common /hf/ cp(72),sp(72)
      common /hl/ nwad, tslope, tsqsl
      real*8 time1, time2
      common /hop/ time1,time2,nopu,notim
      common /hpn/ wslope
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /iclmpq/ lat(nsn),lon(nsn)
      common /logfil/ logfil
      common /idno/ ksel,ksort
      common /igl/ nglobalzs, globalzs(20)
      common /ilv/ c(nsn), e(nsn)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /ihfgpq/ itest(100)
      common /lm/ mapend
      common /ocfn/ adj,seh,lat1,bat2,lon1,bon2,igap
      character*1 isnla, isnlo, krm1, krm2
      common /ocfn1/ isnla, isnlo, krm1, krm2
      common /ohq/ gap, supout
      common /omnfh/ dmin,dmin3,sminp
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /ph/ nfirst
      common /pno/ lph,keyph(npa),nsum
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      character*4 krms
      common /po/ krms(npa)
      common /pqt/ near
      common /pfnoqv/ kdate,khrmn,khr
      common /pgqv/ w(npa)
      common /phoqn/ inst,knst
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pnoqtx/ ldx(npa)
      character*4 krmp
      common /pnoqv/ krmp(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /qgnotx/ delta(npa)
      common /qw/ azmv(5), iuse(npa), duse(npa), ndwt, iboxc
      common /qmost/ wt(npa),z
      common /reloc/ irelo, nreloc
      common /ro/ yse,seorg,phi
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      data sockets/.false./
      if (iterm .gt. 0) then
        sockets = .true.
      endif
      if(iprn .ge. 5) write(punt, '(a)') 'begin subroutine locate'
20    if(itest(38) .eq. 11) itest(38) = 1
c read in data for one earthquake
      call phasin
      needsum = .true.
      instset = ' '
      if((lph .gt. 0) .and. (nr .eq. 0)) then
c case of no phase data
        iexit = 1
        write(logfil, 22) kdate, ihr, kmin
        write(punt, 22) kdate, khrmn/100, khrmn - 100*(khrmn/100)
22      format(' could not locate event of ', i6, ' ', i2, ':', i2)
c---- write out warning messages
        rewind(16)
        read(16, '(a)') erout
2296    read(16, '(a)', end=2298) erout
        if(erout(1:3) .eq. 'end') goto 2298
        write(punt, '(a)') erout
        write(logfil, '(a)') erout
        goto 2296
2298    continue

        if((ipun .eq. 2) .or. (ipun .eq. 3)) call npunch('final')
        if(eoff) goto 180
        goto 20
      endif
c at end of file
      if(eoff) goto 180
c no data here, but try main input file again
      if(nr .eq. 0) goto 20
      if ((nr .eq. 1) .and. (ipro .eq. 'stop')) then
c print summary
        call lissum(2)
c terminate, even if using sockets
        stop
      endif
      if ((nr .eq. 1) .and. (ipro .eq. 'rese')) then
c "reset" record found
        if (ichec .eq. 't s ') then
c "reset s" record found
c print summary
          call lissum(2)
c reset summary
          call lissum(1)
c show elapsed time
          call timit(1)
          time1 = 0.0d0
        endif
c get new parameters
        call input1
        goto 20
      endif
c normal earthquake case
      kkdate = kdate
      khr = ihr
      kmin = lbastm
      sec = 0.0
      dum = 0.0
      call tshift(kkdate,khr,kmin,sec,dum)
      kkyr=kkdate/10000
      kkmo=(kkdate-kkyr*10000)/100
      kkday=(kkdate-kkyr*10000-kkmo*100)
      if(iprn .lt. -1) then
c no print in this case
      else if(iprn .eq. -1) then
c one line per event in output and log file
        write(punt, 24) kkyr,kkmo,kkday,khr,kmin
        write(logfil, 24) kkyr,kkmo,kkday,khr,kmin
24      format(1x, i2.2, '/', i2.2, '/', i2.2 , 4x, i2.2, ':', i2.2,
     *  5x, a)
      else
        write(punt,26)
26      format('                                              ---------',
     * '-------------------------------------------------------',
     *      /'                                              ---------',
     * '-------------------------------------------------------',
     *      /'                                              ---------',
     * '-------------------------------------------------------')
        write(punt,24)kkyr,kkmo,kkday,khr,kmin,iahead
      endif
c if desired, call wadati
      wslope = 0.
      if(test(49) .ne. 0.) then
        call wadati(nr, nrp, w, ldx, tp, kdate, khrmn, iprn,
     *  ilis, krmp, krms, msta, test, wslope, worig, se3)
        if(wslope .ge. 1.2) then
          tslope = tslope + wslope
          tsqsl = tsqsl + wslope*wslope
          nwad = nwad + 1
          if(freor) savor = worig
          if(test(49) .lt. 0.) then
            savor = worig
            inst = 8    
          endif    
        else  
          if((wslope .ne. 0.) .and. (iprn .ge. 1)) then
            write(punt, '(a,/,a)') 
     *      ' VP/VS slope was less than 1.2, so it will not be used to', 
     *      ' set initial origin time nor to compute average VP/VS.' 
	  endif
        endif 
      endif
c find improved first trial location using half space velocity = test(48)
      vhalf = test(48)
      nmax = 10
c if any initial values = 99999. at this time, then compute them from p times!
      if(iprn .ge. 5) write(punt, *) ' savla, savlo = ', savla, savlo
      if( (savla .eq. 99999.) .or. (savez .eq. 99999.) ) then
        if(iprn .ge. 5) then 
          write(punt, *) ' number of p times ', nrp
          write(punt, *) ' p times ', (tp(i), i = 1, nrp)
          write(punt, *) ' weights ', (w(i), i = 1, nrp)
          write(punt, *) ' savla, savlo, savez, savor ',
     *          savla, savlo, savez, savor
          write(punt, *) ' call strtep'
        endif
        npinv = nrp
        if(npinv .gt. 9) npinv = 9
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 28) npinv
28      format(' compute starting parameters from the first ',
     *  i3, ' p-arrival times.')
        call strtep(c, e, kdx, lat(1), lon(1), nrp, nmax, vez,
     *                   vla, vlo, savor, tp, vhalf, w, test(54))
        if(iprn .ge. 5) then 
          write(punt, *) ' strtep input and output:'
          write(punt, *) ' lat(1), lon(1) = ', lat(1)*deg, lon(1)*deg
          write(punt, *) ' nrp, nmax = ', nrp, nmax
          write(punt, *) ' vhalf = ', vhalf
          write(punt, *) ' savor ', savor
          write(punt, *) ' vla, vlo, vez ', vla*deg, vlo*deg, vez
        endif
c
c define savla and savlo
        if(savla .eq. 99999.) then
c in this case, redefine savla and savlo from strtep results
	  if(iprn .ge. 5) 
     *     write(punt, *) 'savla = 99999. so use vla and vlo'
          savla = vla
          savlo = vlo
        endif
c
c set starting depth
        if(savez .eq. 99999.) then
c usedly uses the current values of savla and savlo.
c zuse is set to 99999. by usedly if usedly does not set starting depth.
c if zuse is set to 99999. then test(36) is the maximum starting depth.
cd        write(punt, *) 'call usedly to get starting depth'
          call usedly(0, zuse, modset, savla, savlo)
          if(zuse .ne. 99999.) then
            savez = zuse
          else if(test(5) .ne. -99.) then
            savez = test(5) + test(8)
          else
c use vez determined by strtep
c limit vez to the range 15 to test(36) km and round to the nearest 5 km
            if(vez .gt. test(36) + test(8)) vez = test(36) + test(8)
            if(vez .lt. 15. + test(8)) vez = 15. + test(8)
            nz = vez + .5
            savez = nz
          endif
        endif
      endif
      if(savor .eq. 99999.) savor = 0.
c
c find location for one earthquake
      if(itest(50) .gt. 22) itest(50) = 22
      irep = itest(50)
30    continue
cd    write(punt, *) 'call usedly from statement 91'
      call usedly(0, zuse, modset, savla, savlo)
      if((dnstrg(evtype) .eq. 't') .or. 
     *   (dnstrg(evtype) .eq. 'n') .or. 
     *   (dnstrg(evtype) .eq. 'r')) then
c       teleseism, nuclear or regional
        call velazm
        iexit = 1
        if((ipun .eq. 2) .or. (ipun .eq. 3)) call npunch('final')
        if(eoff) goto 180
        if(ipro .eq. 'more') goto 96
        goto 20
      endif

      if(nglobalzs .ne. 0) then
	call glob_new
	goto 40
      endif

      if((iglob .eq. 0) .and. 
     *   ((inst .eq. 0) .or. (inst .eq. 8))) then
        call global
        if ((nsolut .gt. 1) .and. (iprn .ge. 0) .and. (ilis .gt. 0))
     *  write(punt, 32) nsolut, (altz(i), altrms(i), i = 1, nsolut)
32      format(' the number of solutions = ', i2, /,
     *         '     depth      rms', /,
     *         ( 2f10.2))
        call output(1)
        if(iexit .eq. 0) then
          if((kms.eq.0) .and. (needsum)) call mising
c keep running sum of magnitude residuals for sumout
c but do not include multiple solutions of same event in summary
          if(needsum) call lissum(0)
          if(nfirst .ge. iabs(itest(7)) )  call fmplot
c write summary data to temporary files 14 and 15, call geterr and
c then add the the data to files 4 and 11
          if(ipun .ne. 0) call npunch('temp ')
          call geterr(zup, zdn, rmslim)
c add zup and zdn to summary record and write output to .sum and .arc
cd        write(punt, *) 'call adderr with zup, zdn = ', zup, zdn
          if(ipun .ne. 0) call adderr(zup, zdn)
          if (iprn .ge. 0) write(punt, 34) zup, zdn, yse, rmslim
34        format(' depth may decrease by ', f10.2,
     *    ' or increase by ', f10.2, ' given a reading ', /,
     *    ' standard error of ', f8.3, ' and rms limit of', f8.3)
        else
c iexit = 1, so there is insufficient data for a solution
          if((ipun .eq. 2) .or. (ipun .eq. 3)) then
            call npunch('final')
          endif
        endif
        if((itest(44) .eq. 1) .and. (inst .ne. 9)) then
c since (44) = 1, check if this is a debug event to rerun
          if((nedit .eq. 3) .and. (.not. good)) then
            itest(44) = 100
            inst = 0
            goto 40
          endif
        endif
c check for critical solution based on itest(44) = 2
        if ((itest(44) .eq. 2) .and. (inst .ne. 9)) then
c  continue iteration with only critical stations
          if (iprn .ge. 5) write(punt, '(a)')
     *    ' continue iteration with only critical stations'
          itest(44) = 200
          inst = 0
          goto 40
        endif
        goto 50
      endif
c
c normal solution (not global)
40    call quakes
c if iexit is set = 2 in quakes there are too few phases to
c rerun event with only critical stations.
      if(iexit .eq. 2) then
        if(itest(44) .eq. 100) itest(44) = 1
        if(itest(44) .eq. 200) itest(44) = 2
        goto 50
      endif
      call output(1)
c if iexit is set = 1 in quakes there is insufficient data
      if(iexit .eq. 1) then
        if((ipun .eq. 2) .or. (ipun .eq. 3)) then
          call npunch('final')
        endif
        goto 20
      endif
      if (abs(test(6)) .gt. 0.01) then
        call boxau
      endif
      if(itest(44) .eq. 100) then
        itest(44) = 1
        goto 50
      endif
      if (itest(44) .eq. 200) then
        itest(44) = 2
        goto 50
      endif
      if((kms.eq.0) .and. (needsum) .and.
     * (.not. supout)) call mising
      if( (nfirst .ge. iabs(itest(7))) .and.
     * (.not. supout)) call fmplot
c keep running sum of magnitude residuals for sumout
c do not include multiple solutions of same event in summary
      if((needsum) .and. (.not. supout)) call lissum(0)
      if(ipun .ne. 0) call npunch('final')
      if(itest(50) .ne. 0) then
        if(irep .eq. 0) then
c place a blank record between sets of summary records
          write(4, '(1h1)')
c go on to the next set of phase data
          supout = .false.
          goto 50
        endif
        if(irep .eq. itest(50)) then
c this is the first extra run of this event
c set sepout to true to supress calling output and
c computing magnitude
          supout = .true.
c set inst = 1 to fix z
          inst = 1
c set starting depth to surface of model
          savez = 0.0
          zdif = 1.
        else
c on later extra runs
          savez = savez + zdif
          zdif = 1. + savez/5.
        endif
        irep = irep - 1
        goto 30
      endif
      if((itest(44) .eq. 1) .and. (inst .ne. 9)) then
c since (44) = 1, check if this is a debug event to rerun
        if((nedit .eq. 3) .and. (.not. good)) then
          itest(44) = 100
          goto 30
        endif
      endif
      if((itest(44) .eq. 2) .and. (inst .ne. 9)) then
c since (44) = 2, rerun
        itest(44) = 200
        goto 30
      endif
50    continue
      time1=time2
      if(itest(38) .eq. 1) then
        itest(38) = 11
        knst = 0
        goto 30
      else if(itest(38) .eq. 11) then
        itest(38) = 1
      endif
      if(ipro .ne. 'more') goto 20
c
c read next instruction record
96    read(inpt, '(a)') icard
      phcard(lph) = icard
      if(isa.eq.1) write(1,'(a)') icard
      read(icard, 100)
     *     ipro, ichec,  knst, inst, zres,     ala1, 
     *    isnla,  ala2,    alo1, isnlo,  alo2,      org1, org2
100   format(a4,    a4, 9x,i1,   i1, f5.2, 16x,f2.0,  
     *       a1,  f5.2, 5x,f3.0,    a1,  f5.2, 11x, f2.0, f5.2)
      instset = ' '
      malaz = icard(21:24)
      malla = icard(43:46)
      malor = icard(74:77)
      if(icard(9:9) .ne. ' ') evstat = icard(9:9)
      if(icard(10:10) .ne. ' ') evtype = icard(10:10)
      write(punt,110) ipro, ichec, evstat, evtype,   knst,   inst,
     *     zres,     ala1, isnla, ala2,     alo1, isnlo, alo2, 
     *     org1, org2

110   format(' ipro check evstat evtype knst inst depth   ',
     *' latitude   longitude, origin time', /,
     * 1x,             a4,     a4, 5x,a1, 6x,  a1, 5x, i1, 4x, i1, 
     * 2x, f6.2, 2x, f4.0,    a1, f5.2, 1x, f5.0,    a1, f5.2,
     * 3x, f3.0, f5.2, /, 25x, '--- run again ---  ')

      ipro = dnstrg(ipro)
      ichec = dnstrg(ipro)
      if(itest(38) .eq. 2) knst = 1
      if(itest(38) .eq. 3) knst = 0
c calculate first trial hypocenter
      if(malla .ne. '    ') then
        la = ala1 + .0001
        lo = alo1 + .0001
        call fold2(savla,savlo,la,isnla,ala2,lo,isnlo,alo2)
      endif
      if(malaz .ne. '    ') savez = zres + test(8)
c define starting origin time
      savor = 0.
      freor = .true.
      if(malor .ne. '    ') then
        savor = 60.*(org1 - lbastm) + org2
        freor = .false.
      else if(inst .eq. 8) then
        inst = 0
      endif
c convert inst=7 to inst=9 with origin free - this is
c the option to use for quary blasts with unknown origin time.
      if(inst .eq. 7) then
        inst = 9
        freor = .true.
      endif
      if((dnstrg(evtype) .eq. 't') .or. 
     *   (dnstrg(evtype) .eq. 'n') .or. 
     *   (dnstrg(evtype) .eq. 'r')) then
c       teleseism, nuclear or regional
        call velazm
        iexit = 1
        if((ipun .eq. 2) .or. (ipun .eq. 3)) call npunch('final')
        if(ipro .eq. 'more') goto 96
        goto 20
      endif
      needsum = .false.
      if((ipro .eq. '    ') .or. (ipro .eq. 'more')) goto 30
160   write(punt,170) ipro, ichec
170   format(/' xxxerrorxxx ', 2a4, ' skipped.  must be blank or more,')
      goto 20
c
c after all earthquakes are processed (eoff is true)
180   if(sockets) then
        return
      endif
      call lissum(2)
c     show elapsed time
      call timit(1)
      write(punt, *) 'irelo = ', irelo, ' nreloc = ', nreloc
      if(irelo .gt. 0) then
c relocate events with new station delays irelo times
        nreloc = nreloc + 1
        if(nreloc .gt. irelo) stop
c rewind input(inpt), archive(11), summary(4), output(9)
        write(punt, '(a)') ' rewind files and rerun events'
        if(inpt .eq. 5) then
          write(punt, *) 
     *     ' input is standard input, which can not be rewound'
          write(punt, *) 
     *     ' therefore, the relocate option may not be used'
          stop 'abort from locate'
        endif
        rewind (inpt)
        rewind (11)
        rewind (4)
        rewind (9)
c       reset summary
        call lissum(1)
        eoff = .false.
        time1 = 0.0d0
        call input1
        goto 20
      endif
      return
      end
c end locate
c median.f    []
c	parameter (n = 8)
c	dimension x(n)

c	x(1) = 1.90560
c	x(2) = 1.27506
c	x(3) = 2.35488
c	x(4) = 2.22930
c	x(5) = 1.90560
c	x(6) = 1.75282
c	x(7) = 2.01101
c	x(8) = 1.84767

c	write(6,'(''Data values:'')')
c	write(6,'(4e20.10)') x
c	call median(x,n,xmed,ierr)
c	print *, 'xmed = ', xmed
c	stop
c	end

	subroutine median(x,n,xmed,ierr)

c  use iterative method to determine the median:

c      N                                          __ N     xj
c     ___     xj - xmed                           >     -----------
c     \      ----------- = 0    ==>    xmed =     -- 1  |xj - xmed|
c     /      |xj - xmed|                         -----------------
c     ---                                         __ N      1
c     j=1                                         >     -----------
c                                                 -- 1  |xj - xmed|

c  modified by C. Stephens from routine mdian2 in Numerical Recipes, by
c  Press, Flannery, Teukolsky, and Vetterling, Cambridge Univ Press, 1986,
c  pp. 460-462.

c  the variable nequal was added to ensure convergence in the case where 1 or
c  more data values are equal to the current guess for the median (= a)

c  when more than one data value is equal to the true median and the current
c  guess (= a) is near but not equal to this value, then convergence can be
c  slowed by successive iterations that oscillate about the true median.
c  under these conditions, either xp is equal to xm from the previous iteration,c  or xm is equal xp from the previous iteration.  the new variables xplast and
c  xmlast are used to test for this case.

c  error return code ierr = 0 for no errors
c			  = maxit (= max(100,100*log(n)) if convergence does not
c				  occur within this number of iterations, in
c				  which case xmed is set to the current guess

	integer*4 n
	real*4 x(n)

	integer*4 np, nm, nequal, j, halfn
	real*4 xx, xp, xm, sumx, sum, eps, dum
	real*4 ap, am, aa, a

	parameter (big=1.0e30, afac=1.5, amp=1.5)

	ierr = 0

	a = 0.5*(x(1)+x(n))
	eps = abs(x(n)-x(1))
	am = -big
	ap = big
	halfn = n/2
	xplast = big
	xmlast = -big
	maxit = 100*nint(log(1.*n))
	if (maxit .lt. 100) maxit = 100
	nit = 0

1	nit = nit + 1
	if (nit .gt. maxit) then
	  ierr = maxit 
	  xmed = a
	  return
	endif
	sumx = 0.0
	sum = 0.0
	np = 0
	nm = 0
	nequal = 0
	xm = -big
	xp = big

	j = 0
	do while (j .lt. n)
	  j = j + 1
	  xx = x(j)

	  if (xx .eq. a) then
	    nequal = nequal + 1

	  else

	    if (xx .gt. a) then
	      np = np + 1
	      if (xx .lt. xp) xp = xx

	    else if (xx .lt. a) then
	      nm = nm + 1
	      if (xx .gt. xm) xm = xx
	    endif

	    dum = 1.0/(eps+abs(xx-a))
	    sum = sum + dum
	    sumx = sumx + xx*dum

	  endif

	enddo

c -- check for oscillations about true median

	if (xm .eq. xplast) then
	  a = xm
	  go to 1
	else if (xp .eq. xmlast) then
	  a = xp
	  go to 1
	else
	  xmlast = xm
	  xplast = xp
	endif

c -- adjust xp,np and xm,nm if some data values equal current guess

	if (nequal .gt. 0) then

	  if (np .lt. halfn) then
	    np = np + nequal
	    if (np .gt. halfn) np = halfn
	    xp = a
	  endif

	  if (nm .lt. halfn) then
	    nm = nm + nequal
	    if (nm .gt. halfn) nm = halfn
	    xm = a
	  endif

	endif

c -- check distribution of points about current guess

	if (np-nm .gt. 1) then
c					guess too low
	  am = a
	  aa = xp + max(0., sumx/sum-a)*amp
	  if (aa .gt. ap) aa = 0.5*(a+ap)
	  eps = afac*abs(aa-a)
	  a = aa
	  go to 1

	else if (nm-np .gt. 1) then
c					guess too high
	  ap = a
	  aa = xm + min(0., sumx/sum-a)*amp
	  if (aa .lt. am) aa = 0.5*(a+am)
	  eps = afac*abs(aa-a)
	  a = aa
	  go to 1

c -- success

	else

	  if (mod(n,2) .eq. 0) then
c					n is even
	    if (np .eq. nm) then
	      xmed = 0.5*(xp+xm)
	    else if (np .gt. nm) then
	      xmed = 0.5*(a+xp)
	    else
	      xmed = 0.5*(xm+a)
	    endif

	  else
c					n is odd
	    if (np .eq. nm) then
	      xmed = a
	    else if (np .gt. nm) then
	      xmed = xp
	    else
	      xmed = xm
	    endif

	  endif

	endif

	return

	end
c mising.for    []
      subroutine mising
c check to see if data from stations that were not used in
c------- this solution would significantly improve hypocenter
      include 'params.inc' 
      parameter (ndly = 11)
      real lat,lon,latep,lonep,mag
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /amo/ tempg(npa), ntgap
      common /dmost/ ipun,ivlr,blank
      common /iclmpq/ lat(nsn),lon(nsn)
      common /ilmpu/ ns
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /ilv/ c(nsn),e(nsn)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /iox/ prr(nsn),iuses
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn), xmwt(nsn)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
      common /ip1/ rsew(4)
      common /omnfh/ dmin,dmin3,sminp
      common /pfnoqv/ kdate,khrmn,khr
      common /pm/ jdx(nsn)
      common /pox/ fmp(npa),prx(npa),icent1,icent2
      common /qmost1/ lonep,ni,latep
      common /reloc/ irelo, nreloc
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      common /xfmno/ mag
      ihd=0
      nj = ntgap + 1
      tempg(nj)=tempg(1)+360.
      if (mag .eq. blank) then
        tdel=100.
      else
        tdel = 10.**(1.48 + mag/2.)
      endif
      do 130 i=1,ns
    5 if (ndate(i) .eq. 99999998) goto 130
      if (jdx(i) .eq. 1) goto 130
      call delaz(latep,lonep,deli,deldeg,azi,lat(i),lon(i))
      if (deli .gt. tdel) goto 130
      if (azi .le. tempg(1)) azi=azi+360.
      do 10 j=2,nj
      if (azi .lt. tempg(j)) goto 20
   10 continue
      j=nj
   20 exgap=tempg(j)-tempg(j-1)
      rdgap=tempg(j)-azi
      tgap=azi-tempg(j-1)
      if (tgap .lt. rdgap) rdgap=tgap
      if (deli.gt.dmin3 .and. rdgap.lt.30.) goto 130
      if (azi .ge. 360.) azi=azi-360.
      if (ihd .eq. 1) goto 22
      write(punt,21)
   21 format(/10x,45hmissing station  delta   azim  ex-gap  rd-gap)
      ihd=1
c     make sure station record has not expired.
   22 if (ndate(i) .gt. icent2*1000000+kdate) goto 100
      if ((ndate(i) .eq. icent2*1000000+kdate) .and.
     *  (nhr(i) .ge. khrmn/100)) goto 100
      call update(indexs, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, 
     *         tpdly, revp, ndate, nhr, ns,
     *         exdly, icent2, kdate, khrmn,
     *         infil, iofil)
      goto 5
  100 write(punt,125) nsta(i),deli,azi,exgap,rdgap
  125 format(21x,a5,2f7.1,2f8.1)
  130 continue
      return
      end
c end mising
c openfl.for    [unix]
      subroutine openfl(iunit, ifile, istat, izero, ishr, iform, irecl)
c system dependent program to open files for hypoelllipse
      integer iunit
c             iunit is unit number for this file
      character*(*) ifile
c                   ifile is name of file to be opened
      character*(*) istat
c                   istat is open status ('new' or 'old' or 'scratch'
c                     or 'unknown')
      character*(*) izero
c                   izero must be 'zero' or 'null'; if 'zero' then
c                     open with "blank='zero'"
      character*(*) ishr
c                   ishr indicates share status:
c                     must be 'readonly' or 'none' ('shared' is not coded)
      character*(*) iform
c                   iform = 'noprint' for some files on masscomp
c                     uacal file needs 'unformatted'
c                     otherwise = 'none'
      integer*4     irecl
c                   irecl is used as record length on vax, unless
c                     it is zero.
c
      if (ishr .ne. 'readonly' .and. ishr .ne.
     &       'none') then
        print *, 'openfl: invalid argument for ishr: ', ishr
	print *, 'iunit = ', iunit
	print *, 'ifile = ', ifile
	print *, 'istat = ', istat
	print *, 'izero = ', izero
	print *, 'ishr  = ', ishr
	print *, 'irecl = ', irecl

        stop
      endif
      if (izero .ne. 'zero' .and. izero .ne. 'null') then
        print *, 'openfl: invalid argument for izero: ', izero
        stop
      endif
      if (istat .ne. 'new' .and. istat .ne. 'old' .and.
     &    istat .ne. 'scratch' .and. istat .ne. 'unknown') then
        print *, 'openfl: invalid argument for istat: ', istat
        stop
      endif
c* (vax
c      if (irecl .le. 0) then
c        if (ishr .eq. 'readonly') then
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero', readonly)
c          else
c            open(unit=iunit, file=ifile, status=istat,
c     &      readonly)
c          endif
c        else
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero')
c          else
c            open(unit=iunit, file=ifile, status=istat)
c          endif
c        endif
c      else
c        if (ishr .eq. 'readonly') then
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero', readonly, recl=irecl)
c          else
c            open(unit=iunit, file=ifile, status=istat,
c     &                    readonly, recl=irecl)
c          endif
c        else
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero', recl=irecl)
c          else
c            open(unit=iunit, file=ifile, status=istat,
c     &      recl=irecl)
c          endif
c        endif
c      endif
c* vax)
c* (unix
c#if masscomp
c      if (iform .eq. 'none') then
c#else
      if (iform .eq. 'none' .or. iform .eq. 'noprint') then
c#endif
          if (izero .eq. 'zero') then
            open(unit=iunit, file=ifile, status=istat,
     &      blank='zero')
          else
            open(unit=iunit, file=ifile, status=istat)
          endif
      else
          if (izero .eq. 'zero') then
            open(unit=iunit, file=ifile, status=istat,
     &      blank='zero', form=iform)
          else
            open(unit=iunit, file=ifile, status=istat,
     &      form=iform)
          endif
      endif
c     write (0,'(" trying to open ",a," with status = ",a)')
c    &  ifile, istat
c* unix)
c* (pc
c      if (istat .eq. 'scratch') istat = 'new'
c      if (irecl .le. 0) then
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero')
c          else
c            open(unit=iunit, file=ifile, status=istat)
c          endif
c      else
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero', recl=irecl)
c          else
c            open(unit=iunit, file=ifile, status=istat,
c     &      recl=irecl)
c          endif
c      endif
c* pc)
      return
      end
c end openfl
c phagt.for    [unix]
      subroutine phagt(phcard, keyph, lph, inpt, injump, eoff, icent2,
     *  itest)
c read the data associated with one earthquake
c
c     phcard(npa)     array of records read
c     keyph(npa)      key to record type
c                       0 --- phase record
c                      -1 --- comment record
c                      -2 --- summary record
c                      -3 --- instruction record
c                             (begins with more, dist, or 4 blanks)
c                      -4 --- bad time
c                      -5 --- reset record
c                      -6 --- save record - no longer used
c                      -7 --- rerun record - no longer used
c                      -8 --- scatter record
c                      -9 --- not on station list
c                     -10 --- jump record
c                     -11 --- decode error
c                     -12 --- sleep record
c                     -13 --- stop record
c                     -14 --- nongaus record
c                     -15 --- master scatter record
c     lph             number of records returned
c     inpt            unit number for reading data
c     eoff            .false. --- end of file not reached
c                     .true. --- found end of file
c     npa            maximum number of phases allowed
c
      save onesav
      include 'params.inc' 
      character*10 begtim, dnstrg*4, pname*4, foocard*115, acent2*2
      integer punt, icentsave, icent2, itest(100)
      common /punt/ punt
      common /logfil/ logfil
      common /dmost/ ipun,ivlr,blank
      character*117 phcard(npa), fname*50
      dimension keyph(npa)
      logical onesav, eoff, fsteq
      data onesav/.false./,fsteq/.true./
      eoff = .false.
      icentsave = 0
      npurep = 0
c
      lph = 1
c     write(punt, *) 'onesav = ', onesav
      if (onesav) then
        phcard(1) = phcard(isv)
        keyph(1) = keyph(isv)
        onesav = .false.
        if ((keyph(1) .ne. -2) .and. (keyph(1) .ne. 0)) then
          return
        endif
        goto 25
      endif
c
c loop from lph = 1 to npa
c     print *,'phaget about to read unit ',inpt
15    read(inpt, 20, end = 70) phcard(lph)
20    format(a)
c
c classify record
25    continue
      pname = dnstrg(phcard(lph)(1:4))
c
c comment record
      if (pname(1:2) .eq. 'c*') then
        if( (ipun .eq. 2 .or. ipun .eq. 3) .and.
     *    (lph .eq. 1) .and. (fsteq) ) then
c comment records prior to the first summary or phase record should
c be written directly to the archive phase file
          write(11, '(a)') phcard(lph)
          goto 15
        endif
        keyph(lph) = -1
        goto 50
c
c summary record in 115 column format
      else if ((phcard(lph)(81:81) .eq. '/') .or.
c* (vax
c* (pc
c     *  (phcard(lph)(81:81) .eq. '\')) then
c* pc)
c* vax)
c* (unix
     *  (phcard(lph)(81:81) .eq. '\\')) then
c* unix)
        keyph(lph) = -2
	icent2 = itest(55)
	write(phcard(lph)(116:117), '(i2)') icent2
	if(icentsave .ne. 0) then
c two summary records with different centuries will bomb out the program
	  if(icentsave .ne. icent2) then
	    write(punt, '(a,/)') 
     *        'xxxerrorxxx in phagt.  Two summary records for the ',
     *        'save event with different centuries. ', phcard(lph)
	    write(logfil, '(a,/)') 
     *        'xxxerrorxxx in phagt.  Two summary records for the ',
     *        'save event with different centuries. ', phcard(lph)
	    stop
	  endif
	endif
        icentsave = icent2

        if (npurep .gt. 0) goto 90
        goto 50
c
c summary record in 117 column format
      else if ((phcard(lph)(83:83) .eq. '/') .or.
c* (vax
c* (pc
c     *  (phcard(lph)(83:83) .eq. '\')) then
c* pc)
c* vax)
c* (unix
     *  (phcard(lph)(83:83) .eq. '\\')) then
c* unix)
        keyph(lph) = -2
c must be in new 117-column format, so switch back for internal use
        read(phcard(lph)(1:2), '(i2, t1, a2)') icent2, acent2
        foocard = phcard(lph)(3:117)
        phcard(lph) = foocard//acent2
        if(icentsave .ne. 0) then
c two summary records with different centuries will bomb out the program
          if(icentsave .ne. icent2) then
            write(punt, '(a,/)')
     *        'xxxerrorxxx in phagt.  Two summary records for the ',
     *        'save event with different centuries. ', phcard(lph)
            write(logfil, '(a,/)')
     *        'xxxerrorxxx in phagt.  Two summary records for the ',
     *        'save event with different centuries. ', phcard(lph)
	    stop
          endif
        endif
        icentsave = icent2
 
        if (npurep .gt. 0) goto 90
        goto 50
c
c instruction record
      else if ((pname .eq. '    ') .or.
     *        (pname .eq. 'more')) then
c    *   .or. (pname .eq. 'dist')) then
        keyph(lph) = -3
        goto 80
c
c reset record
      else if (pname .eq. 'rese') then
        keyph(lph) = -5
        if (lph .gt. 1) goto 90
        return
c
c save record
      else if (pname .eq. 'save') then
        keyph(lph) = -6
        if (lph .gt. 1) goto 90
        return
c
c rerun record
      else if (pname .eq. 'reru') then
        keyph(lph) = -7
        if (lph .gt. 1) goto 90
        return
c
c scatter record
      else if (pname .eq. 'scat') then
        keyph(lph) = -8
        if (lph .gt. 1) goto 90
        return
c
c nongaus record
      else if (pname .eq. 'nong') then
        keyph(lph) = -14
        if (lph .gt. 1) goto 90
        return
c
c master scatter record
      else if (pname .eq. 'mast') then
        keyph(lph) = -15
        if (lph .gt. 1) goto 90
        return
c
c sleep record
      else if (pname .eq. 'slee') then
        keyph(lph) = -12
        if (lph .gt. 1) goto 90
        return
c
c stop record
      else if (pname .eq. 'stop') then
        keyph(lph) = -13
        if (lph .gt. 1) goto 90
        return
c
c jump record
      else if (pname .eq. 'jump') then
        keyph(lph) = -10
        fname = phcard(lph)(6:55)
        begtim = phcard(lph)(56:65)
        if (inpt .eq. injump) then
          close (unit = injump, iostat = iostat)
          write(punt, 40)
          write(logfil, 40)
40        format(' xxxerrorxxx can not jump from a jump file.')
          stop
        else
          inpt = injump
          write(punt, 41) fname
          write(logfil, 41) fname
41        format(' jump to ', a)
          close (unit = injump, iostat = iostat)
c         call openfl( iunit, ifile, istat,  izero, ishr,
c    *    iform, irecl)
          call openfl(injump, fname, 'old', 'zero', 'readonly',
     *    'none', 0)
c          open(unit=injump, file=fname, blank='zero', status='old',
c     *    iostat = iostat)
c          if (iostat .ne. 0) goto 498
          if(begtim .ne. ' ') then
            write(punt, 412) begtim
            write(logfil, 412) begtim
412         format(' begin processing with event:  ', a)
            if(lph .ne. 1) then
              write(punt, 413)
              write(logfil, 413)
413           format(' *** jump record must be between events ***', /,
     *        ' *** instruction record missing, so stop ***')
              stop
            endif
            do 417 i = 10, 1, -1
              if(begtim(i:i) .ne. ' ') goto 418
417         continue
418         lenbt = i
420         read(injump, '(a)', end = 430) phcard(lph)
            if(phcard(lph)(1:lenbt) .eq. begtim(1:lenbt)) goto 25
            goto 420
430         write(punt, 435) begtim
            write(logfil, 435) begtim
435         format(' *** could not find ', a, ' so stop ***')
            stop
          endif
        endif
        if (lph .gt. 1) goto 90
        goto 15
c498     write(punt, 499) fname
c        write(logfil, 499) fname
c499     format(' *** jump file ', a, /, ' could not be opened',
c     *  ' *** so stop ***')
c        stop
      endif
c
c must be regular phase record
      keyph(lph) = 0
      npurep = npurep + 1
50    fsteq = .false.
      lph = lph + 1
      if (lph .le. npa) goto 15
c
55    write(logfil, 60) npa
      write(punt, 60) npa
60    format(///,' xxxxx exceeded maximum of ',i4,
     *           ' records per event, so stop!  xxxxx')
      stop
c
c come here on eof detected
70    eoff = .true.
c     write(punt, *) 'just set eoff to true in phagt.  eoff = ', eoff
      onesav = .false.
      if (lph .gt. 1) then
        phcard(lph) = ' '
        keyph(lph) = -3
        write(logfil, 72)
        write(punt, 72)
72      format(//, ' detected end of file prior to final instruction',
     *  ' record.', /, ' check input data for completeness.')
        return
      else
        lph = lph - 1
        return
      endif
c
c come here after an instruction record or other special record
80    continue
      return
c
c come here if missing an instruction record
c     ('add' blank instruction record)
90    isv = lph + 1
      phcard(isv) = phcard(lph)
      keyph(isv) = keyph(lph)
      phcard(lph) = ' '
      keyph(lph) = -3
      onesav = .true.
      write(logfil, 92) phcard(isv)
      write(punt, 92) phcard(isv)
92    format(/, ' xxxxx warning - missing instruction record - xxxxx',
     *       /, ' prior to this record:', /, ' ', a)
      return
      end
c end phagt
c phaind.for    []
      subroutine ceck_staz(msta,nsta,ns,itarg,ierr)
c     returns itarg, the index of nsta that equals target.
c     input msta=sigla
c     input nsta=sigle (vettore)
c     input ns=n stazioni
      character*4 msta
      character*5 nsta(*)
      integer itarg

      ierr = 0
      do i=1,ns
       if (msta .eq. nsta(i)(01:04)) goto 50
      end do
      ierr = 1
      return
50    continue
      itarg=i
      return
      end
c prplot.for    []
      subroutine prplot(x, y, xmax, xmin, ymax, ymin, lines, last,
     *      isym, no, most)
c     written by w. gawthrop
c     plot takes a set of x and y values and plots them on the printer
c     arguments
c           x     the array of x coordinates
c           y     the array of y coordinates
c           xmax  the largest value of x
c           xmin  the smallest value of x
c           ymax  the largest value of y
c           ymin  the smallest value of y
c           lines the number of lines for the y axis
c          igraph the matrix used for storage of the entire graph
c                 before printing
c           last  the number of data points
c           isym  the symbol to be used as a point on the graph
c           no,most     this is curve number no of the maximum number
c                       most to be printed on these axes
c
      character*4 isym(last)
      character*1 igraph(111, 56)
      dimension x(last), y(last), zx(12)
      integer punt
      common /punt/ punt
      yl=ymax
      ys=ymin
      xs=xmin
      xl=xmax
      xscale=(xl-xs)/110.
      a=lines-1
      yscale=(yl-ys)/a
      if (no-1)30,10,30
   10 continue
      do 20 i=1,111
      do 20 j=1,lines
   20 igraph(i,j)= ' '
      do 29 i=1,lines
      igraph(1,i)='i'
   29 igraph(111,i)='i'
c------- form s-p for plotting
   30 do 40 i=1,last
c     y(i)=y(i)-x(i)
      xx=x(i)
      yy=y(i)
  303 if (xl-xx) 40,31,31
   31 if (xx-xs)40,32,32
   32 if (yl-yy) 40,33,33
   33 if (yy-ys) 40,34,34
   34 ix=(xx-xs)/xscale+1.5
      iy=(yy-ys)/yscale+.5
      iy=lines-iy
      if (isym(i) .eq. ' ') goto 40
      if (ix.lt.0.or.ix.gt.111) goto 38
      if (iy.lt.0.or.iy.gt.111) goto 38
      igraph(ix,iy)=isym(i)(1:1)
      if (igraph(ix,iy) .eq. ' ') igraph(ix,iy) = '+'
      igraph(ix+1,iy)=isym(i)(2:2)
      igraph(ix+2,iy)=isym(i)(3:3)
      igraph(ix+3,iy)=isym(i)(4:4)
      goto 40
   38 write(punt,39) isym(i)
   39 format(1x,a4,' plots off of graph')
   40 continue
      if (no-most) 50,51,50
   50 return
   51 do 52 k=1,11
   52 zx(k)=10.*      (k-1)*xscale+xs
      zx(12)=zx(11)+10.*xscale
c     write(punt,1)
      write(punt,2)
      yes=yl+yscale
      do 60 i=1,lines
      yes=yes-yscale
   60 write(punt,4) yes,(igraph(j,i),j=1,111 )
      write(punt,6)
      write(punt,7) (zx(k),k=1,12)
    1 format(1h1)
    2 format(1h1, ' ts ',2x,23('i    '))
    4 format(1h ,f6.2,111a1)
    6 format(1h ,6x,22('i....'),'i')
    7 format(1h ,' tp',f6.2,11f10.2)
   80 return
      end
c end prplot
c qdist.for    []
      subroutine qdist(az, deldeg, delta, duse, idclc, kdx, keyd,
     * lat, lon, latep, lonep, ldx, near,
     * ndwt, nprwt, nrp, nrwt, wt)
c if idclc is true, compute distance and azimuth.  in any case sort by
c distance and set weights equal to reading weights.
      include 'params.inc' 
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
c     real     az(npa)     azimuth
      real     az(npa)
c     real     deldeg      unused delaz argument
      real     deldeg
c     real     delta(npa)  distance (km)
      real     delta(npa)
c     real     dall(npa)   temporary distance array (all stations)
      real     dall(npa)
c     real     duse(npa)   distance array (stations used)
      real     duse(npa)
c     logical  idclc       if true, the compute new distances, otherwise
c                          just initialize weights and sort by distance.
      logical  idclc
c     integer  kdx(npa)    kdx(phase number) = station number.
      integer  kdx(npa)
c     integer  key(npa)    sort key for used stations
      integer  key(npa)
c     integer  keyd(npa)   sort key for all stations
      integer  keyd(npa)
c     integer  keyuse(npa) key to original phase key
      integer  keyuse(npa)
c     real     lat(nsn)    station latitude
      real     lat(nsn)
c     real     lon(nsn)    station longitude
      real     lon(nsn)
c     real     latep       eq latitude
      real     latep
c     real     lonep       eq longitude
      real     lonep
c     integer  ldx(npa)    0 if no s reading, else array index for s
      integer  ldx(npa)
c     integer  near        key to closest station used
      integer  near
c     integer  ndwt        # stations with p or s used
      integer  ndwt
c     integer  nprwt       # stations with p or s last iteration
      integer  nprwt
c     integer  nrp         # of p and s-p phases
      integer  nrp
c     integer  nrwt        # of phases with weight
      integer  nrwt
c     real     wt(npa)     phase weight
      real     wt(npa)
      parameter (ln=3)
c
      nprwt = nrwt
      ndwt = 0
c initialize distance calculations for current epicenter
c calculate epicentral distances and initialize weights
      do 121 i = 1,nrp
        ji = kdx(i)
c   given station lat & lon (radians) get delta (km) & azimuth (deg)
        if (idclc) then
          call delaz(latep,lonep,delta(i),deldeg,az(i),lat(ji),lon(ji))
        endif
        dall(i) = delta(i)
        if (ldx(i) .gt. 0) then
          k = ldx(i)
          delta(k) = delta(i)
          az(k) = az(i)
        endif
        if(wt(i) .ne. 0.0) then
          ndwt = ndwt + 1
          duse(ndwt) = delta(i)
          keyuse(ndwt) = i
        else if(ldx(i) .gt. 0) then
          if(wt(k) .ne. 0.0) then
            ndwt = ndwt + 1
            duse(ndwt) = delta(i)
            keyuse(ndwt) = i
          endif
        endif
121   continue
c
c sort all stations by distance
      call sort(dall, keyd, nrp)
      near = 1
      if(ndwt .eq. 0) then
        ndwt = 1
        return
      end if
      call sort(duse, key, ndwt)
      near = keyuse(key(1))
      return
      end
c end qdist
c quakes.for    []
      subroutine quakes
c controls the iteration for one earthquake.
c arrays
c     at(3)        computed shift if hypocenter -- in rotated coordinate sys
c     az(npa)      azimuth measured clockwise from north of station with
c                  ith index as measured from current trial epicenter.
c     azmv(5)      azimuth's divideing quadrants for azimuthly weighted
c                  jeffreys weighting.
c     b(4)         hypocenter shift computed by regres (longitude, latitude,
c                  depth, in kilometers).
c     delta(npa)   distance to station with ith index.
c     dly(ndly, nsn)  p-phase delay for model i at station j.
c     duse(npa)    distance array (km) only for stations to be used.
c     iqdo(npa)    quadrant assignment in jeffreys weighting - printed in output
c                  after distance.
c     itest(100)   integer versions of test variables -- itest(i) = int(test(i)
c     iuse(npa)    if the critical station option is used, then stations to be
c                  used have iuse = 1 whereas all others have iuse = 0.
c     kdx(npa)     kdx(phase index number) = station index number.
c     keyd(npa)    sort index for all stations - ie the first station
c                  has index keyd(1).
c     klas(5, nsn) station response types.
c     ksmp(npa)    0 if the reading is an smp reading, 1 otherwise.
c     kwr(npa)     character*1 variable printed after residual to indicate
c                  something about the reading or weight.
c     lat(nsn)     latitude of station.
c     ldx(npa)     0 if no s reading, else array index position for s phase.
c     lon(nsn)     longitude of station.
c     msta(npa)    character*5 -- station name.
c     nsta(nsn)    character*5 -- station name.
c     se(4)        standard error ellipsoid semimajor axes, lon, lat, z, t.
c     test(100)    reset test values - see manual for definition.
c     tl(3)        ratio of step to standard error.
c     tp(npa)      arrival times.
c     w(npa)       assigned phase weight.
c     wf(51)       jeffreys weighting function weights.
c     wt(npa)      final weight of phase, including such factors as
c                  distance weight.
c     x(4, npa)    partial derivatives of travel time with respect to
c                  longitude, latitude, depth, and origin time.
c     xmean(4)     xmean(i) = average of x(i, j) over j
c     y(4)         final step in longitude, latitude, depth, and origin time,
c                  including effects of any limitions.
c variables
c type name          attributes    use
c
c r*4  aar           comm          average of the absolute value of residuals
c r*4  absgr                       absolute value of largest component of
c                                  hypocenter shift.
c r*4  absy1                       absolute value of y1
c r*4  absy2                       absolute value of y2
c r*4  absy3                       absolute value of y3
c r*4  adjsq         comm          square of vector hypocenter shift
c                                  (y1**2 + y2**2 + y3**2)
c r*4  aveaj         comm          average of the 3 eigenvalues of ss matrix
c r*4  avr           comm          average residual
c r*4  avrps         comm          average residual of all p and s phases but
c                                  excluding s-p residuals
c r*4  avuse         comm          equals avrps unless origin time is fixed
c                                  (inst = 8) in which case it equals zero
c r*4  avwt          comm          average weight
c r*4  azi                         azimuth of change in location when only
c                                  critical stations are used
c r*4  been          comm          in rplain, compute certain constants if
c                                  been = 0. and then set been to 1.  input1
c                                  resets been to 0.
c r*4  blank         comm          blank = 1.e20.  variables set to blank will
c                                  not be printed in the output.
c r*4  damp          comm          damping constant to be used in inversion.
c r*4  del                         distance solution changes when only critical
c                                  stations are used.
c r*4  deldeg                      unused argument of qdist
c r*4  dmax          comm          maximum distance used in distance weighting
c r*4  dz                          depth change when only critical stations
c                                  are used.
c l*4  freor         comm          true only if origin time is not fixed.
c r*4  gap           comm          azimuthal gap in stations
c char iahead        comm          heading for this run
c i*4  iboxc         comm          used in weight to control boxcar weighting
c char icard         comm          dummy input record used in many subs
c i*4  icrit                       equals 0 normally.  equals 1 when only
c                                  critical stations are being used.
c l*4  idclc                       if true, the compute new distances in qdist.
c                                  otherwise just reset reading wts & sort by
c                                  distance.
c i*4  iexit         comm          normally 0 but set to 1 if no solution can
c                                  be computed.
c i*4  iglob         comm          if 0 find global minimum, dup and dwn.
c                                  otherwise just find normal solution.
c i*4  igo           comm          if igo = 1 and test(47) ne 0 then add
c                                  extra equation to fix solution on a plane.
c i*4  incdmp                      incdmp is the number if times in a row
c                                  damping is increased.
c i*4  infor         comm          if not 0 on return from weighting then
c                                  there are not enough readings for a solution.
c i*4  inst          comm          column 19 of instruction record.
c                                  also set = -9 in boxau for calculation of rms
c i*4  logfil        comm          unit number for printed output (equals 6).
c i*4  iph           comm          controls printing heading for step output.
c i*4  iprn          comm          print control variable from print option.
c i*4  ipun          comm          punch control variable from punch option.
c i*4  ivlr          comm          number of the layer with variable thickness.
c i*4  kno           comm          delay model (1-5) being used.
c i*4  knst          comm          column 18 from instruction record.
c i*4  kz            comm          if zero, then compute full inversion.  if
c                                  1, 2, or 3 fix longitude, latitude, or depth.
c                                  if negative, do not add damping equations.
c r*4  latep         comm          geocentric latitude of hypocenter in radians.
c i*4  lbastm        comm          base origin time in minutes.
c r*4  lonep         comm          longitude of hypocenter in radians.
c i*4  ndwt          comm          number of stations with p or s used.
c i*4  near          comm          index number of nearest staton.
c i*4  ni            comm          counter for the number of iterations.
c i*4  nprwt                       # stations with p or s during last iteration.
c i*4  nr            comm          total # of p, s, and s-p phases.
c i*4  nrp           comm          total # of p and s-p phases.
c i*4  nrwt          comm          total # of phases with weight not equal zero.
c i*4  nsmp          comm          total # of s-p phases.
c i*4  nswt          comm          total # of s phases with weight not zero.
c r*4  oldy1                       previous step in longitude (km).
c r*4  oldy2                       previous step in latitude (km).
c r*4  oldy3                       privious step in depth.
c r*4  org           comm          origin time in seconds with respect to lbastm
c r*4  pdrms         comm          predicted rms after next shift in hypocenter.
c r*4  prms                        previous rms value.
c r*4  ratio                       reduction ratio applied to hypocenter shift
c                                  because depth change exceeded test(22).
c r*4  rms           comm          root mean square residual.
c r*4  savez         comm          initial depth.
c r*4  savla         comm          initial latitude.
c r*4  savlo         comm          initial longitude.
c r*4  savor         comm          initial origin time.
c l*4  supout        comm          if true, supress all output.
c r*4  sv30                        saved value of test(30).
c r*4  svla                        saved value of latitude during critical run.
c r*4  svlo                        saved value of longitude during critical run.
c r*4  svz                         saved value of depth during critical run.
c r*4  xadjsq                      square of vector hypocenter shift
c r*4  z             comm          earthquake depth.
c r*4  zdn           comm          maximum downward shift of depth within 1
c                                  standard deviation limit of rms.
c r*4  zer           comm          not used.
c r*4  zup           comm          maximum upward shift of depth within 1
c                                  standard deviation limit of rms.
      include 'params.inc' 
      parameter (ndly = 11)
      real lat,lon,latep,lonep
      logical idclc, supout, backup
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /anox/ keyd(npa)
      common /bqz/ avrps,avuse
      common /dhin/ iglob, zup, zdn
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      common /ghnq/ iexit
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /ihfgpq/ itest(100)
      common /iclmpq/ lat(nsn),lon(nsn)
      common /iiq/ wf(51)
      common /logfil/ logfil
      common /ohq/ gap, supout
      common /oqr/ y(4),kz,at(3),tl(3),pdrms,b(4)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pgqv/ w(npa)
      common /phoqn/ inst,knst
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pnoqtx/ ldx(npa)
      common /qmost/ wt(npa),z
      common /qmost1/ lonep,ni,latep
      common /qgo/ org
      common /qgnotx/ delta(npa)
      common /qn/ zer
      common /qo/ dmax,ndec,adjsq,iph
      common /qw/ azmv(5), iuse(npa), duse(npa), ndwt, iboxc
      logical ddone, adone, tdone, bdone, jdone
      common /qw1/ ddone, adone, tdone, bdone, jdone
      common /rioq/ been,damp,dmpinc,igo
      common /pqt/ near
      common /rfgnoq/ se(4)
      common /rq/ xmean(4),avwt,aveaj
      common /tmost/ x(4,npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp

c begin code
      if (iprn .ge. 5) write(punt, '(a)') ' begin subroutine quakes'
c -----------------------------------------------------------------------
c initialize various values
c -----------------------------------------------------------------------
      it21 = itest(21)
      backup = .false.
      sv30 = test(30)
      ddone = .false.
      adone = .false.
      tdone = .false.
      bdone = .false.
      jdone = .false.
      incdmp = 0
      idclc = .true.
      icrit = 0
      azmv(1) = 0.0
      do 10 i = 2,5
   10 azmv(i) = azmv(i-1) + 90.
      do 20 i = 1,nr
   20 iuse(i) = 1
      nrwt = 0.0
      avwt = 1.0
      avrps = 0.0
      iexit = 0
      adjsq = 0.
      iph = 0
      prms = 100000.0
      dmpmn = test(35)
      damp = dmpmn
      igo = 1
c -----------------------------------------------------------------------
c use only critical stations unless called from boxaux [inst = -9]
c -----------------------------------------------------------------------
      if ((itest(44) .ge. 100) .and. (inst .ne. -9)) then
        icrit = 1
        if (nrp .le. 6) then
          write(punt, '(a)') ' too few phases to rerun with '
          write(punt, '(a)') '  only critical phases'
          iexit = 2
          return
        endif
        ni = itest(37)
        if(iprn .ge. 5) write(punt, '(a)') ' call subroutine critic'
        call critic(delta,az,w,iuse,nrp,nr,ldx)
c reset weights to reading weights
        do 22 i = 1,nrp
          wt(i) = w(i)*iuse(i)
          kwr(i) = ' '
          if (iuse(i) .eq. 0) kwr(i) = 'x'
          if (ldx(i) .gt. 0) then
            k = ldx(i)
            kwr(k) = ' '
            wt(k) = w(k)*iuse(k)
            if (iuse(k) .eq. 0) kwr(k) = 'x'
            if ( (knst.ne.1) .and. (knst.ne.6) ) wt(k) = 0.
          endif
22      continue
        svz = z
        svla = latep
        svlo = lonep
        if(inst .ne. 9) then
          if (z .lt. 10.) z = 10.
        endif
        call qdist(az, deldeg, delta, duse, .true., kdx, keyd,
     *  lat, lon, latep, lonep, ldx, near,
     *  ndwt, nprwt, nrp, nrwt, wt)
        call trvdrv
        call zstats
        if (nrwt .lt. 3) goto 96
        if (inst .ne. 9) then
          goto 110
        else
c fixed location, so go ahead and compute steps and standard errors
c set damping = 0.0 and kz = 0
          kz = 0
          damp = 0.0
          sv34 = test(34)
          test(34) = -1.0
          call regres
          test(34) = sv34
          return
        endif
      endif

c -----------------------------------------------------------------------
c initialize trial hypocenter
c -----------------------------------------------------------------------
      ni = 1
      if(iprn .ge. 5) write(punt, '(a)')
     *                  ' set ni=1 and init. hypocenter'
      latep = savla
      lonep = savlo
      z = savez
      org = savor
c -----------------------------------------------------------------------
c set weights to reading weights unless called from boxau
c -----------------------------------------------------------------------
      if(inst .ne. -9) then
        if(iprn .ge. 5) write(punt, '(a)') ' set wts to reading wts'
        do 42 i = 1,nrp
          wt(i) = w(i)*iuse(i)
          kwr(i) = ' '
          if (iuse(i) .eq. 0) kwr(i) = 'x'
          if (ldx(i) .gt. 0) then
            k = ldx(i)
            kwr(k) = ' '
            wt(k) = w(k)*iuse(k)
            if (iuse(k) .eq. 0) kwr(k) = 'x'
            if ( (knst.ne.1) .and. (knst.ne.6) ) wt(k) = 0.
          endif
42      continue
        if (iprn .ge. 5) write(punt, '(a)') ' call zstats'
        call zstats
      endif

      if (iprn .ge. 5) write(punt, 99) latep, lonep, z, lbastm
c
c -----------------------------------------------------------------------
c inst = 9 -- fixed solution
c -----------------------------------------------------------------------
      if (iabs(inst) .eq. 9) then
        if(iprn .ge. 5) write(punt, '(a)') ' fixed solution'
        ni = itest(37)
        call qdist(az, deldeg, delta, duse, .true., kdx, keyd,
     *  lat, lon, latep, lonep, ldx, near,
     *  ndwt, nprwt, nrp, nrwt, wt)
        call trvdrv
c do not call weight if called from boxau.  inst = -9
        if(inst .eq. 9) call weight
        if (iprn .ge. 5) then
          write(punt, *) ' freor = ', freor
          write(punt, *) ' org 	 = ', org
	  write(punt, *) ' avrps = ', avrps
	endif
        if (freor) then
          org = org + avrps
          do 92 i = 1, nr
            if (ksmp(i) .eq. 1) x(4,i) = x(4,i) - avrps
92        continue
          if (iprn .ge. 5) write(punt, '(a)')
     *    ' call zstats'
          call zstats
        endif
c return now if called from boxau
        if(inst .eq. -9) return
c otherwise, go ahead and compute steps and standard errors
c   set dmping = 0.0 and kz = 0
c   prevent scaling by setting test(34) = -1.0
        sv34 = test(34)
        test(34) = -1.0
        kz = 0
        damp = 0.0
        call regres
        test(34) = sv34
        return
      endif
c
c -----------------------------------------------------------------------
c exit if there is not enough data to calculate a solution
c -----------------------------------------------------------------------
c     if (nr .lt. 3) then
      if (nr .ge. 3) goto 110
        gap = 0.0
96      continue
        write(punt,99) latep,lonep,z,lbastm
99      format(/'        latep       lonep           z  base time',/
     *  ,1x,3f12.6,i10)
        write(punt,97) nr
97      format(' xxxxx insufficient data with weights not equal',
     *  ' to zero to calculate a solution', /,
     *  1x, i5, ' readings'/)
        iexit = 1
        rms = 0.
        call qdist(az, deldeg, delta, duse, .true., kdx, keyd,
     *  lat, lon, latep, lonep, ldx, near,
     *  ndwt, nprwt, nrp, nrwt, wt)
        return
c     endif
c -----------------------------------------------------------------------
c begin main iteration loop for hypocentral determination
c -----------------------------------------------------------------------
  110 continue
      if (iprn .ge. 5) then
        write(punt, '(a)') ' top of quakes loop'
        write(punt, '(a, e15.7, /, a)')
     *  ' damp = ', damp, ' call qdist next'
      endif
        call qdist(az, deldeg, delta, duse, .true., kdx, keyd,
     *  lat, lon, latep, lonep, ldx, near,
     *  ndwt, nprwt, nrp, nrwt, wt)
c calculate traveltimes and derivatives from hypocenter to station
      if (iprn .ge. 5) write(punt, '(a)')
     *                   ' call trvdrv to compute ttimes'
cd    print *, 'compute travel times'
      call trvdrv
      if (iprn .ge. 5) write(punt, '(a)')
     *' call weight to compute residuals and rms'
      call weight
      if (iprn .ge. 5) write(punt, '(a, f20.4, a, f20.4)')
     *' old rms = ', prms, ' new rms = ', rms
      if (nrwt .lt. 3) goto 96
c----
c check if solution is better than previous one
cd    print *, 'check if solution is better'
cd    print '(a, 3i8)', '  ni, nprwt, nrwt ',
cd   *          ni, nprwt, nrwt
cd    print *, '  prms, rms, damp, backup ', prms, rms, damp, backup
      if (backup) goto 170
      if (nrwt .gt. nprwt) then
        incdmp = 0
        goto 170
      endif
      if (rms .le. prms) then
        incdmp = 0
        if ((ni .gt. 1) .and. (damp .gt. dmpmn)) then
          damp = damp/2.
          if (damp .lt. dmpmn) damp = dmpmn
cd        print *, '  **** decreased (possibly) damp to ', damp
        endif
        goto 170
      endif
cd    print *, 'rms gt prms and nrwt le nprwt '
c allow rms to increase if distance or azimuthal weighting just applied
      if ((ni .eq. test(10)) .or. (ni .eq. test(13))) goto 170
c -----------------------------------------------------------------------
c solution rms increased, so reverse last step and increase damping
c -----------------------------------------------------------------------
c allow an extra iteration to reverse last step (up to test(21)*1.3)
      it21 = it21 + 1
      if(it21 .gt. itest(21)*1.3) it21 = itest(21)*1.3
      if (iprn .ge. 5) write(punt, '(a)')
     *' solution rms increased, so increase damping'
      incdmp = incdmp + 1
      if(incdmp .eq. 3) then
        if (iprn .gt. -2) then
          write(punt, 176)
  176     format(' *** experiencing convergence problems, ')
        endif
      endif
      dmpnew = damp + aveaj
      if(dmpnew .gt. damp*100.) dmpnew = damp*100.
      if(dmpnew .lt. damp*10.) dmpnew = damp*10.
      damp = dmpnew
cd    print *, '  **** increased damp to ', damp
      pdrms = 0.0
      do 177 i = 1,3
        b(i) = blank
  177 continue
      y(1) = -y(1)
      y(2) = -y(2)
      y(3) = -y(3)
      y(4) = -y(1)*xmean(1)-y(2)*xmean(2)-y(3)*xmean(3)
      xadjsq = y(1)**2+y(2)**2+y(3)**2
      backup = .true.
      goto 325
c -----------------------------------------------------------------------
c regression analysis of travel time residuals
c -----------------------------------------------------------------------
  170 continue
      if (iprn .ge. 5) write(punt, '(a)')
     *' compute next step with regres'
      prms = rms
      if (inst .eq. 1) goto 250
      if ((nrwt .eq. 3) .and. (nsmp .lt. 3)) goto 250
c free solution
      if (iprn .ge. 5) write(punt, '(a)')
     *' free solution with kz = 0'
      kz = 0
      call regres
c do not change depth if horizontal change is large
      if (abs(y(1))+abs(y(2)) .lt. test(25)) goto 300
      if (y(1)**2+y(2)**2 .lt. test(25)**2) goto 300
      if (iprn .ge. 5) write(punt, '(a)')
     *' large epicenter change, so call regres with fixed depth'
c fixed depth solution
  250 kz = 3
cd    print *, 'y1, y2 = ', y(1), y(2)
cd    print *, 'large horiz. shift or inst=1,'
cd    print *, 'or too few phases, so fix depth with kz = ', kz
      if (iprn .ge. 5) write(punt, '(a)')
     *' fixed depth solution with kz = 3'
      call regres
c limit focal depth change
  300 oldy1 = y(1)
      oldy2 = y(2)
      oldy3 = y(3)
      absy3 = abs(y(3))
c if damp > dmpmn reduce y by a factor of 2
c     if (damp .gt. dmpmn) then
c       y(1) = y(1)*.5
c       y(2) = y(2)*.5
c       y(3) = y(3)*.5
c     endif  
      if (absy3 .gt. test(22)) then
        ratio = test(22)/absy3
        y(1) = y(1)*ratio
        y(2) = y(2)*ratio
        y(3) = test(22)*sign(1., y(3))
      endif
c limit horizontal adjustment of epicenter
      absy1 = abs(y(1))
      absy2 = abs(y(2))
      if (absy1 .gt. absy2) then
        absgr = absy1
      else
        absgr = absy2
      endif
      if (absgr .gt. test(24)) then
        ratio = test(24)/absgr
        y(1) = y(1)*ratio
        y(2) = y(2)*ratio
        y(3) = y(3)*ratio
      endif
c avoid hypocenter in air
      if ((z+y(3)) .lt. 0.0) then
        y(3) = -z*test(23) + .000001
      endif
c recompute origin time shift
      y(4) = y(4)-(y(3)-oldy3)*xmean(3)-(y(1)-oldy1)*xmean(1)
     * -(y(2)-oldy2)*xmean(2)
      xadjsq = y(1)**2+y(2)**2+y(3)**2
      if (iprn .ge. 5) write(punt, '(a, 4e15.7)')
     *' final y(1), y(2), y(3), y(4) = ', y(1), y(2), y(3), y(4)
      backup = .false.
  325 if (iprn .ge. 1) call output(0)
c terminate iteration if hypocenter adjustment .lt. test(26)
      if (xadjsq .lt. test(26)) then
c if termination occurs before itest(37) iteration,
c set ni = itest(37) and continue
        if (iprn .ge. 5) write(punt, '(a, e10.5, a, e10.5)')
     *   ' adjustment of ', sqrt(xadjsq), ' < sqrt(test(26)) = ',
     *   sqrt(test(26))
        if (ni .ge. itest(37)) goto 500
        ni = itest(37)
        goto 110
      endif
      ni = ni + 1
      if (ni .gt. it21) goto 500
      if (iprn .ge. 5) write(punt, '(a)') 'adjust hypocenter ********'
      call back(y(2), y(1), latep, lonep, latep, lonep)
      z = z + y(3)
      org = org + y(4)
      adjsq = xadjsq
cd    print *, 'ni, prms, rms ', ni, prms, rms
      goto 110
c ---------------------------------------------------------------------
c termination
c ---------------------------------------------------------------------
  500 if (inst .ne. 8) then
c       reset origin time
        org = org + avrps
        do 507 i = 1,nr
          if (ksmp(i) .eq. 1) x(4,i) = x(4,i) - avrps
  507   continue
        if (iprn .ge. 5) write(punt, '(a, e15.7, a, e15.7)')
     *  ' just increased origin time by avrps = ', avrps, ' to ', org
      endif
      if (iprn .ge. 5) write(punt, '(a)')
     *' call zstats'
      call zstats
c compute error estimates by solving full normal equations
      if (iprn .ge. 5) write(punt, '(a)')
     *  ' compute error est by solving full normal equations'
      damp = 0.0
      sv34 = test(34)
      test(34) = -1.0
      if(inst .eq. 1) then
        kz = 3
      else
        kz = 0
      endif
      call regres
      damp = dmpmn
      test(34) = sv34
c ---------------------------------------------------------------------
c if this is the first time through (because igo = 1) & test(30) < 0
c continue run without fixing epicenter
c ---------------------------------------------------------------------
      if ((igo .eq. 1) .and. (test(28) .lt. 0.0) .and.
     * (test(30) .lt. 0.0)) then
        if (iprn .ge. 5) write(punt, '(a)')
     *  ' continue run without fixing epicenter'
        igo = 0
c temporarily change test(30) to 0.0 so epicenter will not be fixed
        test(30) = 0.0
        ni = itest(37)
        ndec = 0
        prms = 100000.
        iph = 0
        avrps = 0.0
        goto 110
      endif
      test(30) = sv30
c ---------------------------------------------------------------------
c if this is the first time through (igo = 1) & 
c test(28) < 0 & test(47) >0, then 
c continue run without fixing on a plane
c ---------------------------------------------------------------------
      if ((igo .eq. 1) .and. (test(28) .lt. 0.0) .and.
     * (test(47) .gt. 0.)) then
        if (iprn .ge. 5) write(punt, '(a)')
     *  ' continue run without fixing on a plane'
        igo = 0
        ni = itest(37)
        ndec = 0
        prms = 100000.
        iph = 0
        avrps = 0.0
        goto 110
      endif
c -----------------------------------------------------------------------
c for critical solution, compare with normal solution
c -----------------------------------------------------------------------
      if (icrit .ne. 0) then
        dz = z - svz
        call delaz(latep,lonep,del,deldeg,azi,svla,svlo)
        azi = azi + 180.
        if (azi .gt. 360.) azi = azi - 360.
        write(punt,2025) del,azi,dz
 2025   format(/,' using only critical stations the solution changes ',
     *  /,' by:   delta   az     z',/,
     *  6x, 3(1x,f5.1),' km'/)
      endif
      return
      end
c end quakes
c ran3.for    []
      function ran3(idum)
c portable random number generator from press and others, "numerical
c recipes, the art of scientific computing"
      implicit real(m)
      parameter (mbig=4000000.,mseed=1618033.,mz=0.,fac=2.5e-7)
c         parameter (mbig=1000000000,mseed=161803398,mz=0,fac=1.e-9)
      dimension ma(55)
      data iff /0/
      if(idum.lt.0.or.iff.eq.0)then
        iff=1
        mj=mseed-iabs(idum)
        mj=mod(mj,mbig)
        ma(55)=mj
        mk=1
        do 11 i=1,54
          ii=mod(21*i,55)
          ma(ii)=mk
          mk=mj-mk
          if(mk.lt.mz)mk=mk+mbig
          mj=ma(ii)
11      continue
        do 13 k=1,4
          do 12 i=1,55
            ma(i)=ma(i)-ma(1+mod(i+30,55))
            if(ma(i).lt.mz)ma(i)=ma(i)+mbig
12        continue
13      continue
        inext=0
        inextp=31
        idum=1
      endif
      inext=inext+1
      if(inext.eq.56)inext=1
      inextp=inextp+1
      if(inextp.eq.56)inextp=1
      mj=ma(inext)-ma(inextp)
      if(mj.lt.mz)mj=mj+mbig
      ma(inext)=mj
      ran3=mj*fac
      return
      end
c end ran3
c rdtmdp.for    []
      subroutine rdtmdp(l, icard, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr,
     *         exdly)
c decode a time dependent station record (icard) for station number l
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (irecsz = 132)
      integer punt
      common /punt/ punt
      common /logfil/ logfil
      real lat,lon
      dimension ielv(nsn),nsta(nsn)
      dimension lat(nsn),lon(nsn)
      dimension c(nsn),e(nsn),sw(nsn),ndate(nsn),nhr(nsn),klas(5, nsn)
      dimension calr(5, nsn),fmgc(nsn),xmgc(nsn),ipcod(nsn),iscod(nsn)
      integer fmwt, xmwt
      dimension fmwt(nsn),xmwt(nsn),tpdly(2,nsn)
      character*1 exdly(4, nsn), revp(6, nsn)
      character icard*(irecsz) 
      character*4 nsta*5, rshft, nsta4*4
      character*1 ins, iew, dnstrg

      read(icard, 334) nsta4, sw(l), klas(1, l), calr(1, l),
     * xmgc(l), xmwt(l), fmwt(l), fmgc(l), ipcod(l), iscod(l),
     * ndate(l), nhr(l), tpdly(1, l), exdly(1, l), exdly(2, l),
     * exdly(3, l), exdly(4, l),  tpdly(2, l), revp(1,l),  revp(2,l),
     * revp(3,l),  revp(4,l), revp(5,l), revp(6,l),
     * klas(2, l), calr(2, l), klas(3, l), calr(3, l),
     * klas(4, l), calr(4, l), klas(5, l), calr(5, l)
  334 format(            a4,   1x, f4.2,          i2,    4x,f5.2,
     *    f4.2,      i1, 1x, i1,    f4.2,       i1,       i1,
     *    4x,i8,     i2,        f4.2,         a1,          a1,
     *          a1,          a1,        f4.2,          a1,          a1,
     *          a1,          a1,        a1,     a1,6x,
     *  4(i2, 4x, f5.2, 1x))
      do 445 kk = 1, 4
	  exdly(kk,l) = dnstrg(exdly(kk,l))
c reduce all sources equivalent to usgs develocorder to 'v'
          if ( exdly(kk,l) .eq. '*' .or. exdly(kk,l) .eq. '1' .or.
     *         exdly(kk,l) .eq. '4')
     *         exdly(kk,l) = 'v'
c reduce all sources equivalent to uagi develocorder to 'f'
          if ( exdly(kk,l) .eq. '%' .or. exdly(kk,l) .eq. 'a')
     *         exdly(kk,l) = 'f'
c reduce all sources equivalent to usgs mag tape to 's'
          if ( exdly(kk,l) .eq. 'e' .or. exdly(kk,l) .eq. '2')
     *         exdly(kk,l) = 's'
c reduce all sources equivalent to Dan/Daq or pc's to 'd'
c         if ( exdly(kk,l) .eq. 'j' .or. exdly(kk,l) .eq. 'x'
c do not include 'p' because yakutat pc is 'p' and has no satellite delay. jcl 6/3/94
c    *       .or. exdly(kk,l) .eq. 'o'
c    *       .or. exdly(kk,l) .eq. 'g' .or. exdly(kk,l) .eq. 'k'
c    *       .or. exdly(kk,l) .eq. 'i' .or. exdly(kk,l) .eq. 'u')
c    *         exdly(kk,l) = 'd'
c reduce all sources equivalent to uagi daq/dan 'd'
          if (exdly(kk,l) .eq. 'j' .or. exdly(kk,l) .eq. 'x') 
     *      exdly(kk,l) = 'd'
c reduce all sources equivalent to uagi pc's to 'g'
          if (exdly(kk,l) .eq. 'o'
     *      .or. exdly(kk,l) .eq. 'k' .or. exdly(kk,l) .eq. 'i'
     *      .or. exdly(kk,l) .eq. 'u') exdly(kk,l) = 'g'

445   continue
      if (icard(119:123) .ne. ' ') then
        read(icard, 335) alat, alon, ielv(l)
  335   format(118x, f5.3, f5.3, i4)
        call unfold2(lat(l),lon(l),llat,ins,dum1,llon,iew,dum2)
        call fold2(lat(l),lon(l),llat,ins,alat,llon,iew,alon)
        call delaz(lat(1),lon(1),delt,deldeg,azz,lat(l),lon(l))
        c(l) = delt*sin(azz*rad)
        e(l) = delt*cos(azz*rad)
      endif
      nsta4 = rshft(nsta4)
      if (nsta4 .ne. nsta(l)(1:4)) then
        write(punt,381) l,nsta(l)(1:4),nsta4
        write(logfil,381) l,nsta(l)(1:4),nsta4
  381   format(///,'  xxxerrorxxx 391 in station list format, so stop',
     *  /, 1x, i5, 'th station ', 2('"', a4, '" '))
        stop 'abort from rdtmdp'
      endif
c fix up default parameters for blank fields on the station record
c station weight
      if (icard(6:9) .eq. '    ') sw(l) = 1.0
c system type
      if (icard(10:11) .eq. '  ') klas(1, l) = 1
c fmag weight
      if (icard(25:25) .eq. ' ') xmwt(l) = 1.
c xmag weight
      if (icard(27:27) .eq. ' ') fmwt(l) = 1.
c fmag coda multiplier
      if (fmgc(l) .eq. 0.) fmgc(l) = 1.
c  pcode weight-code replacement
      if (icard(32:32) .eq. ' ') ipcod(l) = 10
c scode weight-code replacement
      if (icard(33:33) .eq. ' ') iscod(l) = 10
c expiration date
      if (ndate(l) .eq. 0) ndate(l) = 99999999
      if (ndate(l) .eq. 99999999) nhr(l) = 99
c reversal indicators
      do 332 i = 1, 6
        if((revp(i, l) .ne. ' ') .and.
     *     (revp(i, l) .ne. 'n') .and.
     *     (revp(i, l) .ne. 'r') .and.
     *     (revp(i, l) .ne. '+') .and.
     *     (revp(i, l) .ne. '-') .and.
     *     (revp(i, l) .ne. '?')) then
          write(logfil, '(3a, /, a, /, a)')
     *    ' polarity reversal indicator must be n, r, +, -, or ?,',
     *    ' not ', revp(i, l), icard,
     *    ' indicator is being reset to ?'
          revp(i, l) = '?'
        endif
332   continue
      return
      end
c end rdtmdp
c redgap.for    []
      subroutine redgap(iuse,w,wt,kwr,az,keyd,ldx,nrp)
c reweights stations that geduce a gap of more
c than 60 by 30 or more degrees.
      include 'params.inc' 
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /char/ iahead, msta(npa), nsta(nsn), icard
      dimension key(npa), ldx(npa)
      dimension iuse(npa),w(npa),wt(npa),az(npa)
      character*1 kwr(npa)
      dimension demp(npa), keyd(npa)
      data amingap /30./
      nst = 1
      noaz = 0
c loop through stations, collecting those with data to be used
      do 8 i = 1, nrp
        k = ldx(i)
        if (wt(i) .eq. 0.) then
          if (k .eq. 0) then
            goto 8
          else if (wt(k) .eq. 0.) then
            goto 8
          endif
        endif
c       found a station with either p or s used
        noaz = noaz + 1
        demp(noaz) = az(i)
    8 continue
    4 call sort(demp,key,noaz)
      nj = noaz + 1
      demp(nj) = demp(1) + 360.
c loop through stations in order of distance, considering
c     any that were weighted out due to distance
      do 30 n = nst, nrp
        i = keyd(n)
        k = ldx(i)
c       check if p arrival was weighted out due to distance
        if (kwr(i) .eq. 'd') goto 5
c       check if s arrival was weighted out due to distance
        if ( (k .ne. 0) .and. (kwr(k) .eq. 'd') ) goto 5
        goto 30
c       found a station that was weighted out due to distance
    5   azi = az(i)
        if(azi .lt. demp(1)) azi = azi + 360.
        do 10 j = 2,nj
          if(azi .lt. demp(j)) goto 20
   10   continue
        j = nj
   20   exgap = demp(j) - demp(j-1)
        if(exgap .le. 2*amingap) goto 30
        rdgap = demp(j) - azi
        tgap = azi - demp(j-1)
        if(tgap .lt. rdgap) rdgap = tgap
        if(rdgap .lt. amingap) goto 30
        wt(i) = w(i)*iuse(i)*.5
        if(wt(i) .gt. 0.) kwr(i) = 'g'
   25   if(k .ne. 0) then
          wt(k) = w(k)*iuse(k)*.5
          if(wt(k) .gt. 0.) kwr(k) = 'g'
        endif
        if(kwr(i) .eq. 'g') then
          noaz = noaz + 1
          demp(noaz) = az(i)
          nst = n + 1
          goto 4
        endif
        if(k .ne. 0) then
          if(kwr(k) .eq. 'g') then
            noaz = noaz + 1
            demp(noaz) = az(i)
            nst = n + 1
            goto 4
          endif
        endif
   30 continue
      return
      end
c end redgap
c regres.for    []
      subroutine regres
c compute geiger adjustments by regression
c    first rotate coordinates so that the cross-correlation
c    matrix is diagonalized.  output includes orientation and
c    size of error ellipse.
      include 'params.inc' 
      parameter (ndly = 11)
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dmost/ ipun,ivlr,blank
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /oqr/ y(4),kz,at(3),tl(3),pdrms,b(4)
      common /orz/ onf
      common /phoqn/ inst,knst
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /qgnotx/ delta(npa)
      common /qmost/ wt(npa),z
      common /zqr/ fno
      common /rfgnoq/ se(4)
      common /rbno/ idip(3),iaaz(3),a(3,3)
      common /rioq/ been,damp,dmpinc,igo
      common /ro/ yse,seorg,phi
      common /rob/ v(4,4),noaxp
      common /rq/ xmean(4),avwt,aveaj
      common /tmost/ x(4,npa)
      common /tonxb/ t(npa),fms(npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      dimension r(3),prch(npa),h(3,3),hh(3,3)
      dimension xsum(4),sigma(4),std(4)
      dimension svx3(npa), svx2(npa)
      dimension asm1(6), ev(3,3), eval(3)
      dimension cent(4,4), sscol(4), xm(3)

c test(29) is the default vlaue of standard error used when there
c    are no degrees of freedom.  if test(29) is negative it is always 
c    used regardless of the number of degrees of freedom.
c
c nrwt is the total number of readings with weight .gt. zero.
c
c fno is the sum of p, s, and s-p weights.
c     fno may not equal nrwt because the weights are not normalized.
c
c npors is the number of p and s phases.
c
c onf is the sum of p and s wieghts.
c
c kz is the number of the fixed component, if any.
c
c if solution diverged, increase damping. in this case there is no 
c   need to recompute the sum of squares matrix.
c
      nrsave = nr
      if(inst .eq. 9) yse = abs(test(29))
      if(test(34) .ne. -1.0) then
c fix epicenter if test(30) is negative and if not final call
c for error ellipsoid (test(34) = -1.0)
        if(test(30) .lt. 0.) then
	  if(test(30) .eq. -13.) then
c in this unadvertised, temporary test option, fix long and z
            do 101 i = 1, nr
              svx2(i) = x(1, i)
              svx3(i) = x(3, i)
              x(1, i) = 0.0
	      x(3, i) = 0.0
101         continue
	  else if(test(30) .eq. -23.) then
c in this unadvertised, temporary test option, fix long and z
            do 102 i = 1, nr
              svx2(i) = x(2, i)
              svx3(i) = x(3, i)
              x(2, i) = 0.0
	      x(3, i) = 0.0
102         continue
          else
            do 11 i = 1, nr
              svx2(i) = x(1, i)
              svx3(i) = x(2, i)
              x(1, i) = 0.0
              x(2, i) = 0.0
11          continue
          endif
        else

c if kz > 0, fix the component specified by kz
          if(kz .gt. 0) then
            do 12 i = 1, nr
              svx3(i) = x(kz, i)
              x(kz, i) = 0.0
12          continue

c if test(47) ne 0, igo = 1, and kz <= 0,
c then add extra equation to fix on plane
          else if((test(47) .ne. 0.) .and. (igo .eq. 1)) then
            nr = nr + 1
            call rplain
          endif
        endif
      endif
      phi = nrwt

      npors = 0
      do 13 i = 1, nr
        npors = npors + ksmp(i)
13    continue

c reduce deg of freedom by one if origin time will be calculated
      if(npors .ge. 1) phi = phi - 1.
c compute means of p and s data.
      do 14 j = 1, 4
        xsum(j) = 0.0
        xmean(j) = 0.0
        sscol(j) = 0.0
        do 14 k = 1, 4
          v(j, k) = 0.0
14    continue

      if( (npors .ne. 0) .and. (onf .ne. 0.0) .and.
     *  (inst .ne. 8) ) then
        do 15 j = 1, 4
        do 15 i = 1, nr
          xsum(j) = xsum(j) + wt(i)*x(j, i)*ksmp(i)
15      continue
        do 16 j = 1, 4
          xmean(j) = xsum(j)/onf
16      continue
      endif

      if(test(34) .gt. 0) then
c       compute sum of squares of each column, corrected for mean,
c       for purposes of scaling.  skip if test(34) = 0.0 or if on
c       final call to get error ellipsoid (test(34) = -1.0)
        do 18 j = 1, 4
          do 17 i = 1, nr
            sscol(j) = sscol(j) + wt(i)*(x(j, i)-xmean(j)*ksmp(i))*
     *      (x(j, i)-xmean(j)*ksmp(i))
17        continue
          if(sscol(j) .lt. .00000001) sscol(j) = 1.0
18      continue

c       compute inverse of sqrt of sum of products of squares of 
c         each column, to be used in scaling.
        do 19 j = 1, 4
          do 19 k = j, 4
            cent(j, k) = 1./(sqrt(sscol(j))*sqrt(sscol(k)))
19      continue
      else
        do 21 j = 1, 4
          sscol(j) = 1.0
          do 20 k = 1, 4
            cent(j, k) = 1.0
20        continue
21      continue
      endif

c compute normal equations: v is corrected for mean and scaled by cent
      do 23 j = 1, 4
      do 23 k = j, 4
        do 22 i = 1, nr
          v(j, k) = v(j, k) + 
     *             wt(i)*(x(j, i)-xmean(j)*ksmp(i))*
     *                   (x(k, i)-xmean(k)*ksmp(i))*cent(j, k)
22      continue
        v(k, j) = v(j, k)
23    continue

c output original data, means, std. deviations.
c compute sigma and standard deviations.
      if(iprn .ge. 3) then
        do 24 j = 1, 4
          sigma(j) = sqrt(v(j, j))
          if(phi .gt. 0.0) then
            std(j) = sigma(j)/sqrt(fno)
          else
            std(j) = 0.0
          endif
24      continue

        write(punt, '(a, e15.7)') ' depth = ', z
        write(punt, 25)
25      format(
     *   45x, 'original data', /, '  delta(km)     t(sec)',
     *   '    x(1, i)        x(2, i)        ',
     *   'x(3, i)        x(4, i)          wt(i)      ksmp(i)  name', /)
        do 27 i = 1, nr
          write(punt, 26) delta(i), t(i),
     *      (x(j, i), j = 1, 4), wt(i), ksmp(i), msta(i)
26        format(1x, 2e11.4, 5e15.7, i10, 1x, a5)
27      continue
        write(punt, 28) sscol
28      format(' scale ss=', 13x, 4e15.7)
        write(punt, 29) xmean, onf, sigma, std
29      format('   xmean =', 13x, 5e15.7, /'   sigma =', 13x, 4e15.7, /
     *   ' std dev =', 13x, 4e15.7)
      endif

c find solution to upper left 2x2 by determinates
c  scaled
c      dlons = v(1, 4)*v(2, 2) - v(2, 4)*v(1, 2)
c      dlons = dlons/(v(1, 1)*v(2, 2) - v(1, 2)**2)
c      print *, 'dlons, still scaled, = ', dlons
c      dlons = dlons*sqrt(sscol(4)/sscol(1))
c      print *, '2x2 dlon = ', dlons
c      write(punt, '(a, 2f12.4)') ' 2x2 dlon = ', dlons

c find eigenvalues [diagonal elements of asm1(6)] and eigenvectors
c     [ev(3, 3)] for the upper left 3x3 portion of v(4, 4) [sum of squares]
c	print *, 'v(1,1), (1,2), (1,3) ', v(1,1), v(1,2), v(1,3)
c	print *, 'v(2,1), (2,2), (2,3) ', v(2,1), v(2,2), v(2,3)
c	print *, 'v(3,1), (3,2), (3,3) ', v(3,1), v(3,2), v(3,3)
c	print *, 'v(1,4), v(2,4), v(3,4) ', v(1,4), v(2,4), v(3,4)
30    call eigen1(asm1, 3, 4, 3, ev, eval, v, damp)
      do 31 j = 1, 3
      do 31 i = 1, j
        ni = i + (j*j-j)/2
        a(i, j) = asm1(ni)
        a(j, i) = a(i, j)
31    continue
c	print *, 'a(1,1), (1,2), (1,3) ', a(1,1), a(1,2), a(1,3)
c	print *, 'a(2,1), (2,2), (2,3) ', a(2,1), a(2,2), a(2,3)
c	print *, 'a(3,1), (3,2), (3,3) ', a(3,1), a(3,2), a(3,3)
c transform xmean(k) into principle axis system.
c transform the response vector v(j, 4), j = 1, 3 to r(j)
      do 32 j= 1, 3
        xm(j) = 0.0
        r(j) = 0.0
        do 32 k = 1, 3
          xm(j) = xm(j) + ev(k, j)*xmean(k)
          r(j) = r(j) + ev(k, j)*v(k, 4)
32    continue
c	print *, 'r(1), r(2), r(3) ', r(1), r(2), r(3)

c calculate azmuth and dip of principle directions.
      if(test(34) .eq. -1.0) then
        do 33 j = 1, 3
          if(ev(1, j) .eq. 0.) then 
            iaaz(j) = 0
          else
            azq = atan2(-ev(1, j), ev(2, j))*57.29578
            call formf(azq, iaaz(j), 4, 0)
          endif
          d = sqrt(ev(1, j)**2+ev(2, j)**2)
          dpq = atan2(ev(3, j), d)*57.29578
          call formf(dpq, idip(j), 4, 0)
          if(idip(j) .lt. 0.) then
            idip(j) = -idip(j)
            iaaz(j) = iaaz(j) + 180
          endif
          if(iaaz(j) .lt. 0.) then
            iaaz(j) = iaaz(j) + 360.
          endif
33      continue
      endif

c calculate solution vector in transformed coordinates.
      do 34 j = 1, 3
        if(a(j, j) .gt. damp) then
          at(j) = (r(j)/a(j, j))
        else
          at(j) = blank
        endif
34    continue

c calculate final step in original coordinates.
      do 35 j = 1, 3
        b(j) = 0.0
        do 35 k = 1, 3
          if(at(k) .eq. blank) goto 35
          b(j) = b(j) + ev(j, k)*at(k)
35    continue
      do 36 j = 1, 3
        b(j) = b(j)*sqrt(sscol(4)/sscol(j))
36    continue

c calculate origin time correction.
      b(4) = xmean(4) - b(1)*xmean(1)-b(2)*xmean(2)-b(3)*xmean(3)

      do 37 j = 1, 4
        y(j) = b(j)
37    continue

c calculate predicted sum of squares of residuals after move.
c     write(punt, '(a, /, (1x, 4f15.7))') ' cent(i,j) = ', cent
      pss = v(4, 4)/cent(4, 4)
      do 38 j = 1, 3
        phi = phi - 1.
        pss = pss - b(j)*v(j, 4)/cent(j, 4)
38    continue

c calculate predicted rms.
      pdrms = 10.
      if(fno .gt. 0.) pdrms = sqrt(abs(pss/fno))
      if (iprn .ge. 5) write(punt, '(a, /, 4e15.7)')
     *'           damp            pss          pdrms            phi',
     *damp, pss, pdrms, phi

c estimate standard error, yse.
      if(test(29) .lt. 0.0) then
c       always use test(29) for yse if it is negative.
        yse = -test(29)
      else
        if(phi .ge. 1.) then
          yse = sqrt( v(4, 4)*nrwt / (fno*phi) )
          if(yse .lt. test(29)) then
            yse = test(29)
          endif
        else
          yse = test(29)
        endif
      endif

      if (iprn .ge. 5) write(punt, '(a, /, i15, 3e15.7)')
     *'           nrwt            fno            onf            yse',
     *nrwt, fno, onf, yse

c calculate standard errors and ratios of steps to standard error
c at(j) goes into common
      aveaj = 0.0
      do 39 j = 1, 3
        se(j) = 99.0
        if(at(j) .eq. blank) then
          at(j) = 0.0
        else
          se(j) = 1.87*yse/sqrt(a(j, j))
          if(se(j) .gt. 99.0) se(j) = 99.0
        endif
        aveaj = aveaj + a(j, j)
39    continue
      aveaj = aveaj/3.

c calculate se of origin time.  max is 10.
      seorg = 10.
      if ((inst .ne. 8) .and. (onf .gt. 0.0)) then
        vari = 1./onf
        do 40 i = 1, 3
          if(a(i, i) .lt. 0.000001) goto 41
          vari = vari + xm(i)**2/a(i, i)
40      continue
        seorg = yse*sqrt(vari)
        if(seorg .gt. 10.) seorg = 10.
      endif
41    continue

c for expanded output write out eigenvectors (transformation
c    matrix) and transformed equations.
      if(iprn .ge. 5) then
        do 42 i = 1, 3
          do 42 j = 1, 3
            h(i, j)  = 0.0
            hh(i, j) = 0.0
42      continue

        write(punt, 43)
43      format(/, ' normal equations, scaled and corrected for mean')
        do 45 i = 1, 3
          write(punt, 44) (v(i, j), j = 1, 4)
44        format(10x, e15.7, 8hxdlon + , e15.7, 8hxdlat + ,
     *    e15.7, 6hxdz = , e15.7)
45      continue

        write(punt, 49) v(4, 4)
49      format(32x, 
     *   ' corrected & scaled sum of sq of residuals = ', e15.7)
        write(punt, 50)
50      format(/, ' transformation matrix, cols are eigenvectors')
        do 52 i = 1, 3
          write(punt, 51) (ev(i, j), j = 1, 3)
51        format (10x, 3e15.7)
52      continue

c transform the normal equation coeficients to hh(3, 3).
        do 53 i = 1, 3
        do 53 j = 1, 3
        do 53 k = 1, 3
          h(i, j) = h(i, j) + v(i, k)*ev(k, j)
53      continue
        do 54 i = 1, 3
        do 54 j = 1, 3
        do 54 k = 1, 3
          hh(i, j) = hh(i, j) + ev(k, i)*h(k, j)
54      continue
        write(punt, 55)
55      format(/, ' transformed coeficients')
        do 57 i = 1, 3
          write(punt, 56) (hh(i, j), j = 1, 3)
56        format(10x, 3e15.7)
57      continue
c for expanded output write out transformed equations.
        write(punt, 58)  (iaaz(j), idip(j), j = 1, 3)
58      format(/15x, ' azmuth and dip of principal directions ',
     *  /, 10x, 3(5x, i5, 1h/, i3))
        write(punt, 59)
59      format(/10x, ' steps in directions of eigenvectors',
     *  '  step      std error              t')
        do 61 i = 1, 3
          write(punt, 60) i, r(i), a(i,i), at(i), se(i)
60        format
     *    (10x, ' step(', i1, ') = ', e15.7, '/', e15.7, ' = ', 3e15.7)
61      continue

c for expanded output calculate predicted change in residual, (prch(i))
c   these are based on the step to be taken.
        write(punt, 62) b
62      format(/8h dlon = , e15.7, 8h dlat = , e15.7, 6h dz = , e15.7,
     *    6h dt = , e15.7)
        write(punt, 63)
63      format('         current        predicted      predicted      ',
     *  'weight     name', /,
     *       '         residual       tt change      new residual   ')
        xsum(1) = 0.0
        do 66 i = 1, nr
          prch(i) = -b(4)*ksmp(i)
          do 64 j = 1, 3
            prch(i) = prch(i) - x(j, i)*b(j)
64        continue
          anewr = x(4, i) + prch(i)
          xsum(1) = xsum(1) + wt(i)*anewr**2
          write(punt, 65) i, x(4,i), prch(i), anewr, wt(i), msta(i)
65        format(1x, i2, 4f15.7, 1x, a5)
66      continue
        write(punt, 67) v(4, 4), pss, xsum(1)
67      format(/,
     *  ' sum of squares:    original       predicted       linear',
     *  /, 16x, 3e15.7)
      endif

      nr = nrsave
      if(test(30) .lt. 0.) then
        if(test(30) .eq. -13.) then
          do 675 i = 1, nr
            x(1, i) = svx2(i)
            x(3, i) = svx3(i)
675       continue
        else if(test(30) .eq. -23.) then
          do 676 i = 1, nr
            x(2, i) = svx2(i)
            x(3, i) = svx3(i)
676       continue
        else
          do 68 i = 1, nr
            x(1, i) = svx2(i)
            x(2, i) = svx3(i)
68        continue
        endif
      else
        if(kz .gt. 0) then
          do 69 i = 1, nr
            x(kz, i) = svx3(i)
69        continue
        endif
      endif

      if(test(34) .eq. -1.0) then
        do 692 j = 1, 4
          y(j) = 0.0
692     continue
      endif

      return
      end
c end regres
c riorbk.for    []
      subroutine riorbk(x, ix, jfmt, n, nsig)
c     converts a real number (x) to an integer (ix) that may be printed
c       with no decimal point and later read with fn.nsig format
c     if x = '    ' then ix is set = '    ', and an a format is output
c     jfmt is the format to use in writing ix
c     n is field length and can be from 1 to 7
c
      integer punt
      common /punt/ punt
      common /logfil/ logfil
      character*4 ifmt(7), jfmt
      data ifmt/'(i1)','(i2)','(i3)','(i4)','(i5)','(i6)','(i7)'/
      if((n .gt. 7) .or. (n .lt. 1)) goto 1000
      if((nsig .gt. n) .or. (nsig .lt. 0)) goto 2000
      ix = x*10.**nsig + sign(0.50001,x)
      imax = 10**n
      if(ix .ge. imax) ix = imax - 1
      if(ix .le. (-imax/10)) ix = -imax/10 + 1
      jfmt = ifmt(n)
      return
 1000 write(punt,1010) n
      write(logfil,1010) n
 1010 format(' *** the format i',i5,' is not allowed, so stop.')
      goto 3000
 2000 write(punt,2010) n,nsig
      write(logfil,2010) n,nsig
 2010 format(' *** the format f',i5,'.',i5,' is not allowed, so stop.')
 3000 continue
      stop
      end
c end riorbk
c rnd.for    []
      function rnd()
c     acm algorithm 267 by m.c.pike
c     ran3 is a random number generator from the book numerical recipes by
c     press and others that gives a random distribution over the
c     interval 0 to 1.
c     this function computes a normal distribution with mean zero and
c     standard deviation 1.0.
      f = ran3(0)
      x1 = sqrt(-2.*alog(f))
      t = 6.2831853072*ran3(0)
      rnd = x1*sin(t)
      return
      end
c end rnd
c rplain.for    []
      subroutine rplain
c set up one equation to constrain hypocenter to a plane
      save
      include 'params.inc' 
      parameter (pi = 3.1415926)
      parameter (rpd = pi/180.)
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /imost/ test(100)
      common /oqr/ y(4),kz,at(3),tl(3),pdrms,b(4)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /qmost/ wt(npa),z
      common /rioq/ been,damp,dmpinc,igo
      common /tmost/ x(4,npa)
      dimension hh(3)
c
c fix hypocenter on a plane.
      if (been .ne. 1.0) then
        been = 1.0
        az = abs(test(28))
        di = test(30)
        dir = di*rpd
        cir = pi/2.0-dir
        azr = az*rpd
        hh(1) =  cos(cir)*sin(azr)
        hh(2) = -cos(cir)*cos(azr)
        hh(3) =  sin(cir)
      endif
   75 nrm3 = nr
      wt(nrm3) = 1.
      ksmp(nrm3) = 0
      msta(nrm3) = 'fxpl '
      x(1,nrm3) = hh(1)*test(47)
      x(2,nrm3) = hh(2)*test(47)
      x(3,nrm3) = hh(3)*test(47)
      x(4,nrm3) = 0.0
      return
      end
c end rplain
c rshft.for    []
      character*(*) function rshft(array)
c
c  program to right-justify a character variable
c
c  array - a character variable
      character*(*) array
      character*132 temp
c
c  get length of array
c
      n = len(array)
      rshft = array
      if (n .eq. 0) return
c
c  find the position of the first non-blank character from the right
c
      do 10 i = 1, n
        j1 = n -i + 1
        if (array(j1:j1) .ne. ' ') go to 20
   10 continue
c
c  all characters are blank, so return
c
      return
c
c  shift characters to right
c
   20 if (j1 .eq. n) return
      temp(1:j1) = rshft(1:j1)
      rshft(n-j1+1:n) = temp(1:j1)
      rshft(1:n-j1) = ' '
      return
      end
c end rshft
c sinh.for    []
      function sinh(x)
      sinh = (exp(x) - exp(-x))/2.
      return
      end
c end sinh
c sort.for    []
      subroutine sort(x,key,no)
c w.h.k. lee sort
      dimension x(*),key(*)
      if (no .le. 0) return
      do 1 i=1,no
 1       key(i)=i
      mo=no
 2    if (mo-15) 21,21,23
 21   if (mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      goto 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if (x(i)-x(i+mo)) 28,28,27
 27   temp=x(i)
      x(i)=x(i+mo)
      x(i+mo)=temp
      kemp=key(i)
      key(i)=key(i+mo)
      key(i+mo)=kemp
      i=i-mo
      if (i-1) 28,26,26
 28   jo=jo+1
      if (jo-ko) 25,25,2
 29   return
      end
c end sort
c strtep.for    []
      subroutine strtep(c, e, kdx, fstlat,fstlon, nrp, nmax, vez,
     *                   vla, vlo, savor, tp, vhalf, w, shiftmax)
c gather together the first nmax p-times for use in determining starting locatio
c dimensions of tim, x, and y must increase if nmax>10
      include 'params.inc' 
c              c(nsn)       x coordinates of stations wrt first station in list
      real     c(nsn)
c              e(nsn)       y coordinates of stations wrt first station in list
      real     e(nsn)
c              fstlat       latitude of first station in station list
      real     fstlat
c              fstlno       longitude of first station in station list
      real     fstlon
c              kdx(npa)     kdx(phase number) = station number
      integer  kdx(npa)
c              key(npa)     gives order of arrival times
      integer  key(npa)
c              nrp          number of p arrivals
      integer  nrp
c              nmax         maximum number of p arrivals to use
c                           in estimating first trial location
      integer  nmax
c              vez          depth of earthquake
      real     vez
c              vla          latitude of earthquake
      real     vla
c              vlo          longitude of earthquake
      real     vlo
c              savor        origin time - if not 99999., then fix
      real     savor
c              tim(10)      short list of p arrival times
      real     tim(10)
c              tp(npa)      p arrival times
      real     tp(npa)
c              tptmp(npa)   temporary array for tp values
      real     tptmp(npa)
c              vhalf        halfspace velocity
      real     vhalf
c              w(npa)       weight of readings
      real     w(npa)
c              x(10)        x location of staton
      real     x(10)
c              xz           x location of earthquake
      real     xz
c              y(10)        y location of station
      real     y(10)
c              yz           y location of earthquake
      real     yz
c	       shiftmax	    maximum shift of epicenter from closest station
      real     shiftmax
c save initial depth
      vezsave = vez
      do 20 i = 1, nrp
        tptmp(i) = tp(i)
20    continue
c sort tptmp
      call sort(tptmp, key, nrp)
c find the first nmax values
      n = 0
c in case there are not 4 or more p-arrivals and
c in addition there are none with out zero weight, define xz, yz now
      xz = c(kdx(1))
      yz = e(kdx(1))
      do 25 i = 1, nrp
        k = key(i)
        if(w(k) .eq. 0.) goto 25
        n = n + 1
        tim(n) = tp(k)
        x(n) = c(kdx(k))
        y(n) = e(kdx(k))
        if(n .eq. nmax) then
          nuse = nmax
          goto 30
        endif
25    continue
      nuse = n
      if(nuse .lt. 4) then
        if(nuse .ge. 1) then
c         there are not enough p arrivals to compute a starting location,
c         so use the first one with weight not equal to zero.
          xz = x(1)
          yz = y(1)
        endif
        if(savor .eq. 99999.) savor = 0.
cd      print *, 'savor, vhalf = ', savor, vhalf
cd      do 28 i = 1, nuse
cd        print *,  x(i), y(i), i, tim(i), ' 2'
cd28     continue
        goto 50
      endif
30    continue
c
c solve for best halfspace solution, given m p arrival times.  fix origin
c time if savor .ne. 99999.
cd    print *, 'savor, vhalf = ', savor, vhalf
cd    do 40 i = 1, nuse
cd      print *,  x(i), y(i), i, tim(i), ' 1'
cd40   continue
      call halfsp(nuse, tim, savor, vhalf, x, xz, y, yz, vez)

c make sure starting location is within shiftmax km of first station
c if not, put back at location of first station.
      if(sqrt( (x(1) - xz)**2 + (y(1) - yz)**2 ) .gt. shiftmax) then 
        xz = x(1)
        yz = y(1) 
        vez = vezsave 
      endif

c
50    continue
cd    print *, 'computed starting x, y, z, origin time '
cd    ivez = vez + .5
cd    print *,  xz, yz, ivez, savor, ' 3'
c     print *, 'with respect to ', fstlat, fstlon
c
c convert (xz, yz) to (vla, vlo)
      call back(yz, -xz, vla, vlo, fstlat, fstlon)
c     print *, 'starting lat, lon = ', vla, vlo

      return
      end
c end strtep
c sumgap.for    []
      subroutine sumgap (iuse, az, nr, sge72, nge72, w)
c used by critic to compute sum of gaps > 72 deg and the number of
c gaps > 72 deg
      include 'params.inc' 
      dimension iuse(npa),az(npa),aztemp(npa),key(npa),w(npa)
      j = 0
      do 100 i = 1,nr
        if ((iuse(i) .eq. 0) .or. (w(i) .le. 0.)) goto 100
        j = j + 1
        aztemp(j) = az(i)
  100 continue
      call sort(aztemp,key,j)
      k = j + 1
      aztemp(k) = aztemp(1) + 360.
      sge72 = 0.
      nge72 = 0
      do 200 i = 1, j
        gap = aztemp(i+1) - aztemp(i)
        if (gap .le. 72) goto 200
        sge72 = sge72 + gap
        nge72 = nge72 + 1
  200 continue
      return
      end
c end sumgap
c tatime.for    []
      subroutine tatime (delta,depth,vo,ak,ah,tat,toa1)
c model:  linearly increasing velocity over a halfspace
c source: within the layer
c path:   direct wave
c   bill gawthrop  (modified by j.c. lahr  3/6/91)
c     vo   v at surface
c     ak   velocity gradient
c     ah   thickness of linear layer (depth to half space)
      if (delta.ge. 0.1) then
        a = (vo/ak)**2
        b = (vo/ak + depth)**2
        xc = (delta*delta + a - b)/(2.*delta)
        r = sqrt(xc*xc + b)
        toa1 = acos(xc/r)
        if ((r - vo/ak).gt.ah.and.toa1.le.1.57) then
          tat = 900000.
          return
        endif
        c = r*r + delta*xc - xc*xc
        tat = alog((c + r*delta)/(c - r*delta))/(2.*ak)
      else
        tat = alog((vo + depth*ak)/(vo))/ak
        toa1 = 3.1415926
      endif
      return
      end
c end tatime
c tbtime.for    []
      subroutine tbtime (delta,depth,vo,ak,ah,vi,tbt,toa2)
c model:  linearly increasing velocity over a halfspace
c source: within the layer
c path:   refracted wave
c   bill gawthrop  (modified by j.c. lahr  3/6/91)
c     vo   v at surface
c     ak   velocity gradient
c     ah   thickness of linear layer (depth to half space)
c     vi   velocity of half space
      character*55 mess(3)
      integer punt
      common /punt/ punt
      common /logfil/ logfil
      a = (depth + vo/ak)**2
      b = (ah + vo/ak)**2
      c = (vo/ak)**2
      r = vi/ak
      r2 = r*r
      if(b .le. r2) then
        xc1 = sqrt(r2 - a)
        xi = xc1 - sqrt(r2 - b)
        xc2 = sqrt(r2 - c)
        x2 = xc2 - xc1 + xi
        if ((delta - xi - x2) .gt. 0.0) then
          d = r2 + xi*xc1 - xc1*xc1
          e = r2 + x2*xc2 - xc2*xc2
          tbt = alog(
     *     (d + r*xi)*(e + r*x2)/((d - r*xi)*(e - r*x2))
     *          )/(2.*ak) + (delta - xi - x2)/vi
          toa2 = acos(xc1/r)
        else
          tbt = 900000.
        endif
      else
        mess(1) =  ' for a linear increase over halfspace model the'
        mess(2) =  ' velocity at the base of the linear model is '
        mess(3) =  ' greater than the half space velocity, so stop.'
        write(punt, '(a, /, a, /, a)') mess
        write(logfil, '(a, /, a, /, a)') mess
        stop 'abort from tbtime'
      endif
      return
      end
c end tbtime
c tdtime.for    []
      subroutine tdtime (delta, depth, vo, ak, ah, vi, tdt, toa1)
c
c model:  linearly increasing velocity over a halfspace
c source: within the halfspace
c path:   direct wave
c bill gawthrop  (modified by j.c. lahr  3/6/91)
c
c   this sub determines the traveltime of a ray from an origin
c   below the moho in constant velocity material passing thru a crust
c   with velocity increasing linearly with depth
c   bill gawthrop
c     xo   delta
c     yo   depth
c     vo   v at surface
c     ak   velocity gradient
c     ah   thickness of linear layer (depth to half space)
c     vi   velocity of half space
c     tdt
c     toa1
      double precision xp, xp2, xp3, a, b, c, d, xt1, xt2, el
      double precision xpold, xt, xr, chx, denom, xpa, xpb
      double precision r2, r
      integer punt
      common /punt/ punt
c     write(punt, *) 'in tdtime: delta, depth, vo, ak, ah, vi'
c     write(punt, *)  delta, depth, vo, ak, ah, vi 
      if ((delta .lt. .001) .or. (delta/depth .lt. .001)) then
        tdt = (depth - ah)/vi + alog((vo + ak*ah)/(vo))/ak
        toa1 = 3.1415926
c	write(punt, *) 'tdt, toa1 ', tdt, toa1
        return
      endif
      a = (vi/ak)**2
      b = a - (vo/ak)**2
      c = a - (ah + vo/ak)**2
      d = a*(depth - ah)**2
ctest
      iwrt = 0
      chx = 0.d0
    6 continue
ctest
      xp = .8d0*delta
      do 15 i = 1, 25
        xpold = xp
        xp2 = xp*xp
        xp3 = xp2*xp
        xt1 = dsqrt(d/xp2 + b)
        xt2 = dsqrt(d/xp2 + c)
        xt = xp + xt1 - xt2
        xr = xt - delta
ctest
        if (iwrt .eq. 1) write(punt, 100)
     *    i, delta, depth, xr, xpold, chx,
     *    xp, xt1, xt2, xp3
100     format(i5, 8f13.7, f13.2)
ctest
        if (dabs(xr).lt..0001d0) then
          xc = delta - xt1
          r2 = a + d/xp2
          r = dsqrt(r2)
          ta = r2 - delta*xp + delta*xc + xp*xc - xc*xc
          tb = r*delta - r*xp
          el = dsqrt((xp2 + (depth - ah)**2)*1.d0)
c	  write(punt, *) 'xc, r2, r, ta, tb, el ', xc, r2, r, ta, tb, el
          tdt = alog((ta + tb)/(ta - tb))/(2.*ak) + el/vi
          toa1 = 1.5707963 + acos(xp/el)
c	  write(punt, *) 'tdt toa1 ', tdt, toa1
c
          return
        endif
        denom = xp3*xt1
        if (denom .eq. 0.d0) then
          xp = -1.d0
        else
          xpa = d/(xp3*xt1)
          xpb = d/(xp3*xt2)
          denom = 1.d0 - xpa + xpb
          if (denom .eq. 0.) then
            xp = -1.d0
          else
            chx = xr/(1.d0 - xpa + xpb)
            xp = xp - chx
          endif
        endif
   10   continue
        if (xp .lt. 0.000001) xp = xpold/4.
   15 continue
ctest
      tdt = 900000.
      if(iwrt .eq. 1) return
      write(punt, 200) delta, depth, vo, ak, ah, vi, tdt, toa1,
     * xr, xpold, chx, xp, xt1, xt2, xp3
  200 format(' sub. tdtime convergence problem', /,
     * '         delta           z          vo           ak
     *             ah            vi         tdt         toa1', /,
     * 5x, 8f13.7, /,
     *  '    xr          xpold          chx          ',
     *  ' xp          xt1          xt2          xp3', /, 5x, 7f13.7)
      iwrt = 1
      goto 6
ctest
      end
c end tdtime
c timit.for    [unix]
      subroutine timit(ikey)
c non standard code for finding cpu time on a vax/vms computer
c* (vax
c      if (ikey .eq. 0) then
c        handle = 0
c        it = lib$init_timer(handle)
c      else
c        it = lib$show_timer(handle)
c      endif
c* vax)
c* (pc
c* pc)
c* (unix
c* unix)
      return
      end
c end timit
c trvcon.for    []
      subroutine trvcon(delta, zsv, t, ain, dtdd, dtdh,
     *  lbeg, lend, lbegm1, nlayers,
     *  ivlr, ivl, thk, nlay, ldx, wti, wtk,
     *  tid, did, jref, vsq, vsqd, v, vi, f, vs, vt, msta, stzsv)
c compute travel time and derivatives for model with constant
c    velocity layers
c indecies for this crustal structure, model number imod.  (lbegm1 = lbeg-1)
c
c    interface    index            velocity       thickness
c
c----surface------(lbeg)--------------------------------------(lstm1)---
c                                  v(lbeg)        thk(lbeg)
c
c-----------------(lbeg+1)------------------------------------(lst)-----
c                                    station is in this layer
c                                  v(lbeg+1)=vt   thk(lbeg+1)
c
c-----------------(lbeg+2) = (leqm1)--------------------------(lstp1)---
c                                  v(leqmq)       thk(leqm1)
c
c-----------------(lbeg+3) = (leq)--------------------------------------
c
c    epicenter is in this layer    v(leq)=vs       thk(leq)
c
c-----------------(lbeg+4) = (leqp1)------------------------------------
c                                  v(jj)          thk(jj)
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c-----------------(lend-1) = -------------------------------------------
c
c-top of half space-(lend)----------------------------------------------
c                                  v(lend)        infinite thickness
c
c
      include 'params.inc'
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      dimension jref(lmax)
      character*5 msta
      double precision umax, utemp, div, u1, u2, tkj, tkjsq, det, c1,
     *  c2, a, b, ac4, dmb, u0, vsqu, xtest, dlimit, d0, d1, d2, 
     *  ssqu
c     double precision top, bot
      dimension vsq(lmax), vsqu(lmax), vsqd(lmmax, lmax), v(lmax+2)
      dimension vi(lmax), thk(lmax+1)
      dimension tid(lmax, lmmax), did(lmax, lmmax), f(lmax, lmmax)
      dimension didj(lmmax), tidj(lmmax), tr(lmmax), div(lmax)
      integer punt
      common /punt/ punt
      logical flip
      idrt = 0
      tmin = 99999.
      z = zsv
      stz = stzsv
      if(abs(z - stz) .lt. 0.001) then
        stz = z
      endif
c
c decide which is deeper, station or eq
c and if station is deeper, then reverse roles for now so that
c "eq" is always deeper
      if (z .ge. stz) then
        flip = .false.
      else
        flip = .true.
        temp = z
        z = stz
        stz = temp
      endif
cd    print *, 'z = ', z, ', stz = ', stz
c determine which layer contains the hypocenter.
      sum = 0.0
      do 19 leq = lbeg, lend - 1
        sum = sum + thk(leq)
        if (z .le. sum) then
c         the hypocenter is not in the halfspace
c         leq is the layer containing the hypocenter, leqp1 is the one below,
c         and leqm1 is the one above.
          leqp1 = leq + 1
          leqm1 = leq - 1
          leqr = leq - lbegm1
c         tkj is the distance to the boundary above the eq
          tkj = z - sum + thk(leq)
          tkjsq = tkj**2
cd        print *, 'eq is in layer # ', leq, ', ', tkj, ' km below top'
          goto 23
        endif
19    continue
c the hypocenter is in the halfspace
      leq = lend
cd    print *, 'eq is in halfspace, layer ', leq
      leqm1 = leq - 1
      leqr = leq - lbegm1
      tkj = z - sum
      tkjsq = tkj**2
c set weight if this ray was supposed to be refracted
      if (nlay .ne. 0) wti = 0.0
      idrt = 1
c determine which layer contains the station
23    sum = 0.0
      do 24 lst = lbeg, lend - 1
        sum = sum + thk(lst)
        if (stz .le. sum) then
c         the station is not in the halfspace
c         lst is the layer containing the station, lstp1 is the one below,
c         and lstm1 is the one above.
          lstp1 = lst + 1
          lstm1 = lst - 1
          lstr = lst - lbegm1
c         tkjst is the distance to the boundary above the station
          tkjst = stz - sum + thk(lst)
          tkjstsq = tkj**2
cd        print *,
cd   *      'sta is in layer # ', lst, ', ', tkjst, ' km below top'
          goto 25
        endif
24    continue
c the station is in the halfspace
      lst = lend
cd    print *, 'station is in halfspace, layer ', lst
      lstm1 = lst - 1
      lstr = lst - lbegm1
      tkjst = stz - sum
      tkjstsq = tkj**2
      idrt = 1
      if (leq .eq. lst) then
c eq and station in halfspace
        vs = v(leq)
        vt = vs
        goto 43
      endif
c set the velocity at the source to vs and receiver to vt
25    if(flip) then
        vs = v(lst)
        vt = v(leq)
      else
        vs = v(leq)
        vt = v(lst)
      endif
      tjljll = tid(leq, leqr)
      djljll = did(leq, leqr)
c     correct for variable layers
      if ((ivlr .gt. 0) .and. (leq .gt. ivl) .and.
     *    (jref(leq) .ne. 0)) then
        tjljll = tjljll + thk(ivl)*vsqd(leqr, ivl)*vi(leq)
        djljll = djljll + thk(ivl)/vsqd(leqr, ivl)
        if (leq .gt. ivl + 1) then
          tjljll = tjljll + thk(ivl+1)*vsqd(leqr, ivl+1)*vi(leq)
          djljll = djljll + thk(ivl+1)/vsqd(leqr, ivl+1)
        endif
      endif
c     correct for portion of layer above station
      if ((leq .gt. lst) .and. (vsqd(leqr, lst) .ne. 0.0)) then
        tjljll = tjljll - tkjst*vsqd(leqr, lst)*vi(leq)
cd      print *, 'vsqd(leqr, lst), leqr, lst ', vsqd(leqr, lst), leqr,
cd   *    lst
        djljll = djljll - tkjst/vsqd(leqr, lst)
      endif
c     correct for full layers above station
      if(lst .gt. lbeg) then
        do 30 layer = lbeg, lst - 1
          if (vsqd(leqr, layer) .eq. 0.0) goto 30
          tjljll = tjljll - thk(layer)*vsqd(leqr, layer)*vi(leq)
          djljll = djljll - thk(layer)/vsqd(leqr, layer)
30      continue
      endif
      if ((idrt .eq. 1) .or. (delta .lt. .0001)) goto 43
c***************************************************************************
c calculations for refracted waves.
c***************************************************************************
c if this arrival is to be refracted along a certain boundary
c   calculate travel time or set weight = 0 if this refracted wave
c   can not exist.
      if (nlay .ne. 0) then
        k = nlay + 1
c       k at non-refraction boundary
        if (jref(lbeg + nlay) .eq. 0) goto 31
c       k below(?) halfspace or k above hypocenter layer
        if ((k .gt. nlayers) .or. (k .lt. leqr)) goto 31
c       refraction in layer of hypocenter (compute for direct wave)
        if (k .eq. leqr) then
          idrt = 1
          goto 43
        endif
        didj(k) = did(leq, k)-tkj/vsqd(k, leq)
        if (ivlr .gt. 0) then
          if (k .gt. ivlr) didj(k) = didj(k) +
     *      f(ivl, leqr)*thk(ivl)/vsqd(k, ivl)
          if (k .gt. ivlr+1)
     *      didj(k) = didj(k) + f(ivl+1, leqr)*thk(ivl+1)/
     *      vsqd(k, ivl+1)
        endif
        if (didj(k) .gt. delta) goto 31
        kk = nlay + lbegm1 + 1
        tmin = tid(leq, k )-tkj*vsqd(k, leq)*vi(kk) + delta*vi(kk)
        if (ivlr .le. 0) goto 42
        if (k .gt. ivlr)
     *    tmin = tmin+f(ivl, leqr)*thk(ivl)*vsqd(k, ivl)*vi(k+lbegm1)
        if (k .gt. ivlr+1) tmin = tmin + f(ivl+1, leqr)*thk(ivl+1)*
     *    vsqd(k, ivl+1)*vi(k+lbegm1)
        goto 42
c       if not possible, set p & s weights to zero.
31      wti = 0.0
        if (ldx .ne. 0) wtk = 0.0
      endif
c calculate intercept times and critical distances for all possible
c refracted waves.
      do 34 m = leqp1, lend
        mm = m - lbegm1
        tidj(mm) = 0.
        didj(mm) = 0.
        if (jref(m) .eq. 0) goto 34
        tidj(mm) = tid(leq, mm) - tkj*vsqd(mm, leq)*vi(m)
        didj(mm) = did(leq, mm) - tkj/vsqd(mm, leq)
c       correct for portion of layer above station
        tidj(mm) = tidj(mm) - tkjst*vsqd(mm, lst)*vi(m)
        didj(mm) = didj(mm) - tkjst/vsqd(mm, lst)
c       correct for full layers above station
        if(lst .gt. lbeg) then
          do 32 layer = lbeg, lst - 1
            tidj(mm) = tidj(mm) - thk(layer)*vsqd(mm, layer)*vi(m)
            didj(mm) = didj(mm) - thk(layer)/vsqd(mm, layer)
32        continue
        endif
        if ((ivlr .le. 0) .or. (m .le. ivl)) goto 34
        xtemp = f(ivl, leqr)*thk(ivl)
        tidj(mm) = tidj(mm)+xtemp*vsqd(mm, ivl)*vi(m)
        didj(mm) = didj(mm)+xtemp/vsqd(mm, ivl)
        if (m .le. ivl+1) goto 34
        xtemp = f(ivl+1, leqr)*thk(ivl+1)
        tidj(mm) = tidj(mm) + xtemp*vsqd(mm, ivl+1)*vi(m)
        didj(mm) = didj(mm) + xtemp/vsqd(mm, ivl+1)
34    continue
c xovmax is the maximum cross over distance.  at this or greater
c distance, refracted wave must be first.
      xovmax = (tidj(leqr + 1)-tjljll)/(vi(leq)-vi(leqp1))
cd    print *, 'xovmax = ', xovmax
      if (jref(leqp1) .eq. 0 .or. jref(leq) .eq. 0) xovmax = 9999.
c calculate all possible refracted wave travel-times and find least
      do 40 m = leqr + 1, nlayers
        if (delta .lt. didj(m)) goto 40
        if (jref(m+lbegm1) .eq. 0) goto 40
        tr(m) = tidj(m)+delta*vi(m+lbegm1)
        if (tr(m) .ge. tmin) goto 40
        k = m
        kk = k + lbegm1
        tmin = tr(m)
40    continue
cd    print *, 'min refracted tt and layer = ', tmin, k
c     if (delta .ge. xovmax) then
      if (delta .lt. xovmax) goto 43
c refracted wave is fastest
c relabel travel-time and find derivatives
42      t = tmin
cd      print *, 'refracted wave is fastest'
        dtdd = vi(kk)
        if(flip) then
          dtdh = -vsqd(k, lst)*dtdd
          anin = v(lst)*vi(kk)
        else
          dtdh = -vsqd(k, leq)*dtdd
          anin = v(leq)*vi(kk)
        endif
        ain = deg*asin(anin)
        return
c     endif
c*************************************************************************
c calculations for the direct waves
c*************************************************************************
43    if (leq .eq. lst) then
c for hypocenter and station in same layer
cd      print *, 'hypocenter & station in same layer'
        sqt = sqrt((stz - z)**2 + delta**2)
        tdj1 = sqt/v(leq)
        if (tdj1 .ge. tmin) goto 42
        t = tdj1
        if (sqt .lt. .0001) then
c         hypocentral distance (sqt) less than 0.1 meters
          dtdd = vi(leq)
          dtdh = vi(leq)
          ain = 90.
        else
          dtdd = delta*vi(leq)/sqt
          anin = delta/(sqt)
          if(flip) then
c           downgoing
            dtdh = -(z - stz)*vi(leq)/sqt
            ain = deg*asin(anin)
cd          print *, 'downgoing ain = ', ain
          else
c           upgoing
            dtdh = (z - stz)*vi(leq)/sqt
            ain = 180. - deg*asin(anin)
cd          print *, 'upgoing ain = ', ain
          endif
        endif
        return
      endif
c find parameters of direct wave from hypocenter not in station layer.
c invert the funct. delta(u) (where u is the sine of the angle
c    of incidence) by iterating to find u.
c    use the interpolating funct.:
c       d(u) = c/(umax - u) + a*(u - umax) + b
c    to find better values of u0 which satisfy
c    delta(u0) = given distance within some error, say .03 km.
c use a linear interpolating finction for large epicenteral
c    distances to save computing time.
c----
c find umax for this layer and velocity structure.
c (umax is less than 1 for a hypocenter in a low velocity layer)
      if (delta .lt. 0.0001) then
c vertical ray path
cd      print *, 'vertical ray path'
        u0 = 0.0
        ssqu = 1.d0
        umax = 1.0
        vsqu(lst) = (thk(lst)-tkjst)/(v(leq)/v(lst))
        if(lst+1 .le. leqm1) then
          do 52 l = lst + 1, leqm1
            vsqu(l) = thk(l)/(v(leq)/v(l))
52        continue
        endif
        goto 220
      endif
c non-vertical ray path
cd    print *, 'non-vertical ray path'
      umax = v(leq)/v(lst)
      do 54 l = lst, leqm1
        utemp = v(leq)/v(l)
        div(l) = utemp**2
        if (utemp .lt. umax) umax = utemp
54    continue
cd    print *, 'non-vertical ray.  umax = ', umax
      if (umax .gt. 1.d0) umax = 1.d0
c choose 2 initail points (u1, d1) and (u2, d2) for interpolation,
c    depending on epicentral distance.
      u1 = .75d0*umax
      u2 = delta/sqrt((z-stz)**2+delta**2)
      if (u2 .gt. umax-1.d-4) u2 = umax-1.d-4
      if (u2 .gt. u1) goto 56
      utemp = u1
      u1 = u2
      u2 = utemp
56    continue
c first take care of eq layer and station layer:
      d1 = tkj*u1/dsqrt(1.d0-u1**2) +
     *     (thk(lst) - tkjst)*u1/dsqrt(  vsq(leq)/vsq(lst) - u1**2 )
c    *     (thk(lst) - tkjst)*u1/dsqrt( (v(leq)/v(lst))**2 - u1**2 )
      d2 = tkj*u2/dsqrt(1.-u2**2) +
     *     (thk(lst) - tkjst)*u2/dsqrt(  vsq(leq)/vsq(lst) - u2**2 )
c    *     (thk(lst) - tkjst)*u2/dsqrt( (v(leq)/v(lst))**2 - u2**2 )
c then add terms for layers in between:
      if(lst+1 .le. leqm1) then
        do 58 l = lst+1, leqm1
          d1 = d1 + u1*thk(l)/dsqrt(  vsq(leq)/vsq(l) - u1**2 )
c         d1 = d1 + u1*thk(l)/dsqrt( (v(leq)/v(l))**2 - u1**2 )
          d2 = d2 + u2*thk(l)/dsqrt(  vsq(leq)/vsq(l) - u2**2 )
c         d2 = d2 + u2*thk(l)/dsqrt( (v(leq)/v(l))**2 - u2**2 )
58      continue
      endif
c begin the iteration loop.
      do 150 ll = 1, 25
cd      print *, 'u1, d1, u2, d2 ', u1, d1, u2, d2
c now find the constants a, b, c of d(u) subject to the conditions
c       d(u1) = d1, d(u2) = d2, and d(0) = 0
        det = u1*u2*(u2-u1)
        c1 = d1*u2*(umax-u1)
        c2 = d2*u1*(umax-u2)
        a = (c1-c2)/det
        b = (c1*(2.d0*umax-u2)-c2*(2.d0*umax-u1))/det
        ac4 = 4.d0*a*(a*umax**2-b*umax)
        dmb = delta - b
c invert d(u0) for u0 and find d0 = delta(u0)
        u0 = umax+(dmb-dsqrt(dmb**2+ac4))/(2.d0*a)
        ssqu = dsqrt(1. - u0**2)
        if (u0 .ge. .99999d0) then
c if wave leaves hypocenter nearly horizontally (ie u goes to 1)
c    then compute tdir as wave travelling along top of layer leq
cd        print *, 'compute tdir as wave along top of eq layer'
          tdir = tjljll + delta*vi(leq)
          if (tdir .lt. tmin) then
c           travel time derivatives for hypocenter at top of a layer
            t = tdir
            if(flip) then
c             downgoing
              anin = v(lst)*vi(leq)
              ain =  deg*asin(anin)
              dtdh = -sqrt(1. - anin**2)*vi(lst)
              dtdd = vi(leq)
            else
c             upgoing
              ain = 90.
              dtdh = 0.0
              dtdd = vi(leq)
            endif
            return
          endif
          goto 42
        endif
        if (u0 .ge. umax-1.d-6) then
          u0 = umax-1.d-6
          ssqu = dsqrt(1. - u0**2)
        endif
        d0 = tkj*u0/ssqu
        vsqu(lst) = (thk(lst) - tkjst)/dsqrt( vsq(leq)/vsq(lst)-u0**2)
c       vsqu(lst) = (thk(lst) - tkjst)/dsqrt((v(leq)/v(lst))**2-u0**2)
        d0 = d0 + u0*vsqu(lst)
        if(lst+1 .le. leqm1) then
          do 112 l = lst+1, leqm1
            vsqu(l) = thk(l)/dsqrt( vsq(leq)/vsq(l)-u0**2)
c           vsqu(l) = thk(l)/dsqrt((v(leq)/v(l))**2-u0**2)
            d0 = d0 + u0*vsqu(l)
  112     continue
        endif
c set the distance accuracy of iteration.
        dlimit = .04d0
        if (u0 .lt. .05d0 ) goto 113
        dlimit = .015d0*v(leq)/u0
c test to see if new ray is within required distance accuracy.
  113   xtest = dabs(delta-d0)
        if (xtest  .lt.  dlimit) then
cd        print *, 'trvcon converged direct ray in ', ll, ' iterations'
          goto 220
        endif
c prepare to iterate again
c replace (u1, d1) or (u2, d2) by the new point (u0, d0)
        if (delta .lt. d2) goto 114
        d1 = d2
        u1 = u2
        d2 = d0
        u2 = u0
        goto 150
  114   if (delta .gt. d1) goto 116
        d2 = d1
        u2 = u1
        d1 = d0
        u1 = u0
        goto 150
  116   if (delta .lt. d0) goto 118
        d1 = d0
        u1 = u0
        goto 150
  118   d2 = d0
        u2 = u0
  150 continue
c if solution of direct wave does not converge in 25 iterations
      uu = asin(u0)*deg
      if (uu .lt. 0.0) uu = uu + 180.
      uu = 180. - uu
      write(punt, 172) msta, delta, z, uu, xtest, dlimit
  172 format(' travel-time solution for a direct wave from station ',
     *  a5, ' at distance ', f6.2, ' from depth ', f6.2,
     *  / ' emerging at an angle of ', f10.4,
     *  ' iterated to the limit.', /,
     *  ' came within ', f10.4, ' km.  xlimit = ', f10.4/)
c travel time & derivatives for direct wave below first layer.
  220 continue
      tdir = tkj*vi(leq)/ssqu
      do 240 l = lst, leqm1
        tdir = tdir + v(leq)*vsqu(l)/vsq(l)
240   continue
cd    print *, 'compare tdir and tmin ', tdir, tmin
      if (tdir .ge. tmin) goto 42
c     if refracted wave is faster than direct wave goto 42
c direct wave is faster
cd    print *, 'direct wave is faster than refracted'
      t = tdir
cd    print *, 'umax, u0 ', umax, u0
      if (umax-u0 .lt. .00001) then
cd      print *, 'umax - u0 .lt. .00001 '
cd      print *,
cd   *    'asin(umax), asin(u0) ', deg*asin(umax), asin(u0)
c       horizontal ray at lower limit
        if(flip) then
c         downgoing
          anin = v(lst)*vi(leq)
          ain = deg*asin(anin)
          dtdh = -sqrt(1. - anin**2)*vi(lst)
          dtdd = vi(leq)
        else
c         upgoing
          ain = 90.
          dtdh = 0.0
          dtdd = vi(leq)
        endif
        return
      endif
c compute partial derivative wrt distance (is independent of flip)
c     bot = 0.d0
c     top = 0.d0
c     do 250 l = lst, leqm1
c       term = (vsq(leq)*vsqu(l)**3)/(vsq(l)*thk(l)**2)
c       bot = bot + term
c       top = top + term*u0/v(leq)
c50   continue
c     dtdd1 = top/bot
c     simplify the formula to:
      dtdd = abs(u0)*vi(leq)
cd    print *, 'flip = ', flip
      if(flip) then
c       downgoing
cd	print *, 'downgoing, lst = ', lst
        anin = abs(u0*v(lst)*vi(leq))
        ain = deg*asin(anin)
        ssqu = sqrt(1.0 - anin**2)
c       dtdh1 = (v(lst)*anin*dtdd - 1.0)/(ssqu*v(lst))
c       simplify the formula to:
        dtdh = -sqrt(1. - anin**2)*vi(lst)
c	print *, 'dtdh fancy, plain = ', dtdh1, dtdh
      else
c       upgoing
cd	print *, 'upgoing, leq = ', leq
        anin = abs(u0)
        ain = 180. - deg*asin(anin)
c       dtdh1 = (1.0 - v(leq)*u0*dtdd)/(ssqu*v(leq))
c       simplify the formula to:
        dtdh = sqrt(1. - anin**2)*vi(leq)
c	print *, 'dtdh fancy, plain = ', dtdh1, dtdh
      endif
      return
      end
c end trvcon
c trvdrv.for    []
      subroutine trvdrv
c---- compute travel time and derivatives from crustal model
      save
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      logical tttset, notified
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dit/ tid(lmax,lmmax),did(lmax,lmmax),lowv,modin(mmax+3)
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /idt/ v(lmax2)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /iot/ flt(2,nsn),thks(nsn)
      common /ilotv/ elvdly(npa)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /it/ vpvs(lmax2),vpvsm(mmax+3),nttab
      common /logfil/ logfil
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pnoqtx/ ldx(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /pqt/near
      common /pt/ nlay(npa)
      common /qmost/ wt(npa),z
      real latep, lonep
      common /qmost1/ lonep,ni,latep
      common /qgo/ org
      common /qgnotx/ delta(npa)
      common /tmost/ x(4,npa)
      common /obcfn/ ain(npa)
      common /tonxb/ t(npa),fms(npa)
      common /lvz/ jref(lmax)
      data notified/.false./
      imod = 0
      nearsta = kdx(near)
      call usedly(nearsta, zuse, modset, latep, lonep)
      tttset = .false.
      zpre = -1.e22
      do 30 idum = 1,nrp
      i = idum
      elvdly(i) = 0.0
      ista = kdx(i)
      if(modset .ne. 0) then
        imod = modset
      else
        imod = mod(ista)
      endif
cds      if((delta(i) .gt. test(51)) .and. (nttab .ne. 0)) imod = 11
      if((delta(i) .gt. test(51)) .and. (nttab .ne. 0)) imod = mmax + 1
cds   write(punt, *) 'delta=', delta(i), ' test(51)=', test(51),
cds  * ' nttab=', nttab
      model(i) = imod
      ismod = 0
cds      if (imod .gt. 10) then
cds   write(punt, *) 'trvdrv mmax=', mmax, ' imod=', imod
      if (imod .gt. mmax) then
c       first deal with the p phase **********************************
        if(.not. tttset) then
c         initialize all travel time tables for current depth
          tttset = .true.
          if(z .eq. zpre) go to 14
          zpre = z
          if(z .ge. 0.) then
	    call hyset(nttab, z)
	  else
	    call hyset(nttab, 0.0)
	  endif

          if((test(8) .ne. 0.0) .and. (.not. notified)) then
	    notified = .true.
            write(punt, 10)
            write(logfil, 10)
10          format('Warning:  the elevation of the top of the velocity',
     *        ' models (test(8)) ', /,
     *        'is not zero.  ',
     *        'This is OK for travel times to distant stations.')
          endif

        endif
c                  modin = 1,2, or 3
14	continue
cds	  write(punt, *) 'near statement 14'
cds          write(punt, *) 'calling hytab.  imod=', imod,
cds  *      ' modin(imod)=', modin(imod)
        call hytab(modin(imod), delta(i), t(i), ain(i), dtdd,
     *               dtdh, vt, vs)
        ista = kdx(i)
c       set elevation delay for p phase
        elvdly(i) = 0.0
        if (test(2) .ge. 0.0) then
          if (test(2) .gt. 0.0) then
            tes2 = test(2)
          else
            tes2 = vt
          endif
          utop = (sin(rad*ain(i))*tes2/vs)**2.0
          if(utop .gt. 1.) then
            write(punt, 20) ista,utop,i,ain(i),tes2,vs
            elvdly(i) = 0.0
          else
            elvdly(i) = (ielv(ista)/1000.)*(sqrt(1.0001-utop))/tes2
          endif
        endif
c       p phase derivatives
        x(1,i) = +dtdd*sin(az(i)*rad)
        x(2,i) = -dtdd*cos(az(i)*rad)
        x(3,i) = dtdh
        x(4,i) = tp(i) - t(i) - org - dly(kno,ista) - elvdly(i)
c       now for the s phase ******************************************
        if (ldx(i) .eq. 0) goto 30
        k = ldx(i)
        vpvsmk = vpvsm(imod)
        if (vpvsmk .lt. 0.0) then
c         use travel time table modin(imod) + 1 for the s phase
          model(k) = imod + 1
cds       write(punt, *) "calling hytab.  k=", k, ' imod=', imod,
cds  *      ' modin(imod)=', modin(imod)
          call hytab(modin(imod) + 1, delta(i), t(k),
     *               ain(k), dtdd, dtdh, vts, vss)
          x(1,k) = +dtdd*sin(az(i)*rad)
          x(2,k) = -dtdd*cos(az(i)*rad)
          x(3,k) = dtdh
          vpvsmk = vt/vts
        else
c         assume a constant vp/vs ratio for the s phase parameters
          t(k) = t(i)*vpvsmk
          model(k) = model(i)
          x(1,k) = x(1,i)*vpvsmk
          x(2,k) = x(2,i)*vpvsmk
          x(3,k) = x(3,i)*vpvsmk
          ain(k) = ain(i)
          vts = vt/vpvsmk
          vss = vs/vpvsmk
        endif
c       set elevation delay for s phase
        elvdly(k) = 0.0
        if (test(2) .ge. 0.0) then
          if (test(2) .gt. 0.0) then
            tes2 = test(2)/vpvsmk
          else
            tes2 = vts
          endif
          utop = (sin(rad*ain(k))*tes2/vss)**2.0
          if(utop .gt. 1.) then
            write(punt, 20) ista,utop,k,ain(k),tes2,vss
            elvdly(k) = 0.0
          else
            elvdly(k) = (ielv(ista)/1000.)*(sqrt(1.0001-utop))/tes2
          endif
        endif
        x(4,k) = tp(k) - t(k) - org - sdly(kno,ista) - elvdly(k)
        goto 30
      endif
c**********************************************************************
c     layer model calculations begin here *****************************
      vpvsk = vpvsm(imod)
      if(vpvsk .eq. 0.) vpvsk = test(1)
c     test(8) is the elevation in km of the top of the model
      stz = test(8) - ielv(ista)*.001
      if(stz .lt. 0.0) stz = 0.0
15    nlayers = lend(imod) - lbeg(imod) + 1
c     write(punt, *) 'trvdrv, imod = ', imod, ' lend(imod) = ',
c    *  lend(imod), ' v = ', v(lend(imod))
      if(v(lend(imod)) .lt. 100.) then
c----
c----   constant velocity within each layer with topography
c     variables associated with the variable layer thickness option
c     ivl              layer with variable thickness (=0 to skip this option)
c     vthk(1/2, ista)  model 1/2 thickness assigned to station ista (each
c                        station is assigned two thicknesses)
c     ipthk(ista)      preferred thickness model of station ista
c     lowv             1/0  do/don't make compensating change in thichkess
c                        of layer below the variable layer
c     ivway            neg/0/pos  source/average/receiver station variable
c                        thickness is used
        ivl = lbeg(imod) + ivlr - 1
        if(ivlr .gt. 0) then
c----     determine thickness of the variable layer.
c----     ivway :::  negative-source, positive-receiver, zer0-average
          if(ivway .lt. 0) then
            thk(ivl) = vthk(ipthk(nearsta), ista)
          else if(ivway .gt. 0) then
            thk(ivl) = vthk(ipthk(ista), ista)
          else
            thk(ivl) =
     *        (vthk(ipthk(nearsta),nearsta) +
     *        vthk(ipthk(nearsta),ista))*0.5
            sum = 0.0
            do 16 l = lbeg(imod),ivl
              sum = sum+thk(l)
16          continue
            if (z .ge. sum) then
              thk(ivl) = vthk(ipthk(nearsta),ista)
            endif
          endif
          thk(ivl + 1) = sthk1(imod) - lowv*(thk(ivl) - sthk(imod))
          if(thk(ivl+1) .le. 0.0) thk(ivl+1) = 0.01
          thks(i) = thk(ivl)
        endif
        k = ldx(i)
        if (k .ne. 0) wtk = wt(k)
        call trvcon( delta(i), z, t(i), ain(i), dtdd, dtdh,
     *            lbeg(imod), lend(imod), lbeg(imod)-1, nlayers,
     *            ivlr, ivl, thk, nlay(i), ldx(i), wt(i), wtk,
     *            tid, did, jref, vsq, vsqd, v, vi, f,
     *            vs, vt, msta(i), stz)
        if (k .ne. 0) wt(k) = wtk
      else if(v(lend(imod)) .eq. 100.) then
c----
c----   linear increase, no half space
        write(punt, 17)
        write(logfil, 17)
17      format(' a linear increase with no half space is ',
     *   ' no longer allowed, so stop')
        stop 'abort from trvdrv'
      else if(v(lend(imod)) .eq. 200.) then
c----
c----   linear increase over half space with topography
        vzero = v(lbeg(imod))
        grad  = v(lbeg(imod) + 1)
        zhalf = v(lbeg(imod) + 2)
        vhalf = v(lbeg(imod) + 3)
c       linvol incorporates station elevations
c       print *, '1: i, delta(i), z, stz ', i, delta(i), z, stz
        call linvol(delta(i), z, stz, grad, vzero, zhalf,
     *    vhalf, t(i), ain(i), dtdd, dtdh)
        vt = vzero + stz*z
        vs = vzero + grad*z
        if(z .gt. zhalf) vs = vhalf
      else if(v(lend(imod)) .eq. 300.) then
c----
c----   linear increase with topography
        vzero = v(lbeg(imod))
        grad  = v(lbeg(imod) + 1)
        call linv(delta(i),   z, vzero, grad,
     *    t(i), ain(i), dtdd, dtdh, stz, vt, vs)
cd    else if(v(lend(imod)) .eq. 400.) then
c----
c----   lienert velocity routine with embedded stations allowed
c     dimension parm(lmax2)
c   inputs:    delta  distance from eq to station in km
c              stz    depth of station in km below top of model
c              eqz    depth of eq in km below top of model
c              nn     number of parameters ( = 2*nl - 1)
c              parm(i), i = 1, nl   layer velocities
c              parm(i), i = nl + 1, nn  layer thicknesses
c   outputs:
c              tmin   minimum travel time
c              dtdd   partial derivative of tt wrt distance (s/km)
c              dtdh   partial derivative of tt wrt depth (s/km)
c              ian    angle of incidence in degrees
c       the last layer is excluded, so
cd      nlayers = lend(imod) - lbeg(imod)
cd      do 18 itmp = 1, nlayers
cd        parm(itmp) = v(lbeg(imod) + itmp - 1)
cd        if(itmp .lt. nlayers)
cd   *      parm(itmp+nlayers) = thk(lbeg(imod) + itmp - 1)
cd18    continue
cd      call dtdx2(2*nlayers-1, parm, delta(i), z , stz, t(i),
cd   *  dtdd, dtdh, ain(i), vt, vs)
      else
        write(punt, 19) imod, v(lend(imod))
        write(logfil, 19) imod, v(lend(imod))
19      format(' halfspace velocity for model ', i2, ' may not equal ',
     *    f10.2, /, 'so stop!')
        stop 'abort from trvdrv'
      endif
c----
      elvdly(i) = 0.0
c     if test(8) is non zero, then elevations are accounted for by
c       the location of the stations within the model.
      if ((test(2) .ge. 0.0) .and. (test(8) .eq. 0.0)) then
        if(test(2) .gt. 0.0) then
c----     elevation correction using test(2) as surface velocity
          if(ismod .eq. 1) then
            tes2 = tes2/vpvsk
          else
            tes2 = test(2)
          endif
        else
c----     elevation correction using 1'st layer velocity as surface velocity
          tes2 = vt
        endif
        utop = (sin(rad*ain(i))*tes2/vs)**2.
        if(utop .gt. 1.) then
          write(punt, 20) ista,utop,i,ain(i),tes2,vs
20        format(' sub trvdrv: velocity for elev correction',
     *      ' should not be greater than velocity at source.', /,
     *      ' for ista =',i5,' utop =',f12.7,' ain(',i5,') =',f12.7,
     *     /' v for elev cor =',f12.7,' and v at source =',f12.7)
          elvdly(i) = 0.
        else
          elvdly(i) = (ielv(ista)/1000.)*(sqrt(1.0001-utop))/tes2
        endif
      endif
c---- calculate the partial derivatives of t wrt lon, lat, & z
c----    and find the residuals.
c----
c---- p phase derivatives
      x(1,i) = +dtdd*sin(az(i)*rad)
      x(2,i) = -dtdd*cos(az(i)*rad)
      x(3,i) = dtdh
      if(ismod .eq. 0) go to 27
c---- s phase model just used
      x(4,i) = tp(i) - t(i) - org - sdly(kno,ista) - elvdly(i)
      go to 30
27    if(ksmp(i) .eq. 0) go to 29
c---- p phase residuals
      x(4,i) = tp(i) - t(i) - org - dly(kno,ista) - elvdly(i)
      if(ldx(i).eq.0) go to 30
c---- check for s model
      if(vpvsm(imod) .eq. 0.0) then
        ismod = 1
        i = ldx(i)
        imod = imod + 1
        model(i) = imod
        go to 15
      endif
c---- s phase data -- based on constant vpvs
      k = ldx(i)
      model(k) = imod
      elvdly(k) = elvdly(i)*vpvsk
      t(k) = t(i)*vpvsk
      x(1,k) = x(1,i)*vpvsk
      x(2,k) = x(2,i)*vpvsk
      x(3,k) = x(3,i)*vpvsk
      x(4,k) = tp(k) - t(k) - org - sdly(kno,ista) - elvdly(k)
      ain(k) = ain(i)
      go to 30
c---- s minus p data
29    x(1,i) = x(1,i)*(vpvsk-1.)
      x(2,i) = x(2,i)*(vpvsk-1.)
      x(3,i) = x(3,i)*(vpvsk-1.)
      x(4,i) = ts(i)-tp(i)-(vpvsk-1.)*(t(i)+elvdly(i))
     *  + dly(kno,ista) - sdly(kno,ista)
30    continue
      return
      end
c end trvdrv
c tshift.for    []
      subroutine tshift(kdate,khr,kmin,sec,ssec)
c resolve unreasonable times
    5 if (sec .lt. 60.0) goto 20
      sec = sec - 60.0
      ssec = ssec - 60.0
      kmin = kmin + 1
      goto 5
   20 if (sec .ge. 0.0) goto 40
      sec = sec + 60.0
      ssec = ssec + 60.0
      kmin = kmin - 1
      goto 20
   40 if (kmin .lt. 60) goto 60
      kmin = kmin - 60
      khr = khr + 1
      goto 40
   60 if (kmin .ge. 0) goto 80
      kmin = kmin + 60
      khr = khr - 1
      goto 60
   80 if (khr .lt. 25) goto 100
      khr = khr - 24
      kdate = kdate + 1
      goto 80
  100 if (khr .ge. 0) goto 120
      khr = khr + 24
      kdate = kdate - 1
      goto 100
  120 return
      end
c end tshift
c uamag.for    [unix]
      subroutine uamag (icent2, msta, source, date, dstsqr,
     &  amplit, period, magnit, clusei, sysmag, gndmot,
     & wa_static_mag, punt, blank)
c originally written by steve a. estes long, long ago.
c file fields: msta, start date, stop date, mag for
c   periods .6, .5, .4, .3, .2, .1
c updated 10/22/81 by sae to increase deminsion of
c   f, lsta, date1, date2 to 500.
c updated 12/09/82 by ghcs to be compatible with hypoe82.
c updated 03/23/88 by jcl to incorporate into hypoellipse.
c   stations with system response code = 18 will use this
c   subroutine for xmag calc.
c re-written 09/21/88 by ghcs:  generalized the calibration data to
c   include an arbitrary number of period, magnification pairs for
c   each station entry.  the program was also restructured in fortran
c   77 and separate routines were created:  get_uacaldata() for input
c   and log_interp() for interpolation.
c once more 5/17/89 by jcl.
c   modified to be compatible with vax/vms fortran, which can not read
c   a free format character variable unless the variable is enclosed
c   in quotes.  format must now be a3 or a4 station name followed by
c   one space and then the a1 source code.
c   removed implicit undefined statements.
c   make all source comparisons in upper case.
c   corrected loga0_tbl(2,2) from 50 to 35
c updated 10/20/89 by ghcs to use unformatted data file via the
c   get_bin_uacaldata() routine.
c 6/6/91 by jcl
c   extended loga0_tbl down to 0.1 km distance, to make this routine
c   compatible with xfmags, and so that magnitudes can be computed at
c   close distances.
c 12/23/91 by jcl
c   msta and amp_source are lower case, so use dnstrn in comparison
c 7/15/95 correct interpolation period in wood_anderson table
c 10/22/95 eliminate wood_anderson interpolation and use function wa_magn
c 5/30/97 extented loga0_tbl to 1500 km distance.  jcl
c 2/24/99 add icent2 to keep track of century.  jcl

      save station_list, amp_source, nrecords, npoints,
     &     begin_date, end_date, period_sysmag, 
     &     loga0_tbl
      integer max_records, max_stacode, max_points, max_wapoints
      integer max_loga0pts
      integer punt
c* (vax
c* (unix
      parameter (max_records = 1000)
c* unix)
c* vax)
c* (pc
c      parameter (max_records = 100)
c* pc)
      parameter (max_stacode = 4)
      parameter (max_points = 30)
      parameter (max_wapoints = 4)
      parameter (max_loga0pts = 4)
      integer data_unit
      parameter (data_unit = 24)
c --- declare the subroutine arguments:
      character msta*(5)
      integer date
      real dstsqr, amplit, period, magnit, clusei
c --- declare the common variables:
      character*50 uacal
      common /dix/ iexcal, uacal
c --- declare the local variables:
      character*4 dnstrg
      character station_list(max_records)*(max_stacode)
      character amp_source(max_records)*1, source*1
      integer i, j, k, nrecords, npoints(max_records)
      integer begin_date(max_records), end_date(max_records)
      integer beg_cent, end_cent
      real period_sysmag(2,max_points,max_records)
c     real wood_anderson(2,max_wapoints)
      real loga0_tbl(2,max_loga0pts)
c     real zmc1, zmc2, pwc1, pwc2
      real loga, loga0, sysmag, wa_mag, km, slope, delta, x1, x2
c --- log-log interpolation function.
      real log_interp	
c --- are the periods here given in backwards order???
c     data wood_anderson/.1, 2787.5, .2, 2747.1, .3, 2671.5,
c    &                   .4, 2553.4, .5, 2391.0, .6, 2192.2/
c correct 3rd period 7/15/95 from .8 to .6
c    &                    .8, 2192.2, 42.4, 1.0/
c compute wood anderson magnification rather than extrapolate
c starting 10/22/95
c     data wood_anderson /.01, 2800., .1, 2787.5,
c    &                    .6, 2192.2, 42.4, 1.0/
c     data loga0_tbl /10.,1.5, 50.,2.3, 200.,3.5, 600.,4.9/
c     data loga0_tbl /10.,1.5, 35.,2.3, 200.,3.5, 600.,4.9/
c     data loga0_tbl /0.1,-1.75, 35.,2.3, 200.,3.5, 600.,4.9/
      data loga0_tbl /0.1,-1.75, 35.,2.3, 200.,3.5, 1500.,6.15/
c     data zmc1/0.15/, zmc2/3.38/, pwc1/0.80/, pwc2/1.50/
      data nrecords/-1/
c     write (punt,*) 
c    & 'msta, source, date, dstsqr, amplit, period, magnit, clusei'
c     write (punt,*) msta, source, date, dstsqr,
c    &                    amplit, period, magnit, clusei
c a source of '%' is presumed to be the same as 'a'
      sysmag = blank
      gndmot = blank
      if (source .eq. '%') source = 'a'
c **********************************************
c *** correction for magnification different ***
c *** from 2800.                             ***
c     amag_cor = wamag/2800.
c ***                                        ***
c **********************************************
c *** the first time the routine is called, ***
c *** read in the initial calibration data. ***
c *** Units of sysmag are "units"/mm.       ***
c *** Currently (1991) units are counts/mm  ***
c *********************************************
      if (nrecords.eq.-1)  then
cd        print *, 'about to open ', uacal
cd        print *, 'for unformatted read on unit ', data_unit
c* (unix
        call openfl(data_unit, uacal, 'old', 'null', 'none',
     *    'unformatted', 0)
        print *, 'about to call getbin to read unit ', data_unit
        call getbin (data_unit,
     &  max_records, max_points, nrecords, station_list, amp_source,
     &  begin_date, end_date, npoints, period_sysmag)
c* unix)
c* (vax
c* (pc
c        call get_uacaldata (uacal, max_records,
c     &  max_points, nrecords, station_list, amp_source, begin_date,
c     &  end_date, npoints, period_sysmag, data_unit, punt)
c* pc)
c* vax)
        print *, 'just read ', nrecords, ' records'
        close (data_unit)
      endif
c ***********************************************
c *** convert the distance squared into km.   ***
c *** also, we only want to calculate a mag   ***
c *** for dstsqrs defined in the loga0_tbl    ***
c *** & periods defined in the wood anderson  ***
c *** table.                                  ***
c ***********************************************
      km = sqrt(dstsqr)
c     if (km.lt.loga0_tbl(1,1) .or.
c    &    km.gt.loga0_tbl(1,max_loga0pts)) then
c       clusei = -1
c       return
c     end if
c     if (period.lt.wood_anderson(1,1) .or.
c    &    period.gt.wood_anderson(1,max_wapoints) ) then
      if (period .le. 0.0) then
        clusei = -1
        return
      end if
c *******************************************************
c *** find this station's current calibration record. ***
c *** this should be done much more efficiently!      ***
c *******************************************************
      do 50 k=1, nrecords
c      print *, 'station = ', dnstrg(station_list(k)), msta 
c      print *, 'source = ', dnstrg(amp_source(k)), source
        if (end_date(k) .eq. 999999) end_date(k) = 699999
        if ((dnstrg(station_list(k)) .eq. msta(1:4)) .and.
     &      (dnstrg(amp_source(k))   .eq. source)) then
c if the year is less than 70, then this must be after 1999
	  if (begin_date(k)/10000 .lt. 70) then
	     beg_cent = 20
	  else
	     beg_cent = 19
	  endif
	  if (end_date(k)/10000 .lt. 70) then
	     end_cent = 20
	  else
	     end_cent = 19
	  endif
          if (  (
     *            ((beg_cent .eq. icent2) .and. 
     *	           (begin_date(k) .le. date)) .or.
     *	          (beg_cent .lt. icent2)
     *          ) 
     *          .and.
     *          (  
     *            ((end_cent .eq. icent2) .and.
     *             (end_date(k) .ge. date)) .or.
     *            (end_cent .gt. icent2)
     *          )
     *       ) goto 100
        endif
50    continue
c --- no calibration data matches
      write (punt,'('' no calibration data for /'',a4,''/ source='',a1,
     &  '' date='',i2.2,i6.6)') msta(1:4), source, icent2, date
      clusei = -1.0
      return	
c ***********************************************************
c *** find the system magnification for the given period. ***
c ***********************************************************
100   if (period.lt.period_sysmag(1,1,k) .or.
     &    period.gt.period_sysmag(1,npoints(k),k) ) then
        write (punt,
     &    '('' no calibration data for /'',a4,''/ source='',a1,
     &    '' date='',i6.6,i2,'' at period='',f5.2)')
     &    msta(1:4), source, date, icent2, period
        clusei = -1
        return
      end if
      do 120 i=1, npoints(k)-1
        j = i + 1
        if (period.eq.period_sysmag(1,i,k)) then
c         we have an exact period match, so don't interpolate
          sysmag = period_sysmag(2,i,k)
          goto 200
        else if (period.eq.period_sysmag(1,j,k)) then
c         we have an exact period match, so don't interpolate
          sysmag = period_sysmag(2,j,k)
          goto 200
        else if (period.gt.period_sysmag(1,i,k) .and.
     &           period.lt.period_sysmag(1,j,k)) then
c         we have a period within our defined range, so interpolate.
          sysmag = log_interp (period,
     &               period_sysmag(1,i,k), period_sysmag(2,i,k),
     &               period_sysmag(1,j,k), period_sysmag(2,j,k))
          if (sysmag.lt.0.0) then
            write (punt,'('' sysmag.lt.0 for '',a4)') msta(1:4)
            clusei = -1.0
            return
          endif
          goto 200
        end if
120   continue
c --- we should never be able to get here.
      clusei = -1
      return
c *************************************************************
c *** find the wood anderson response for the given period. ***
c *************************************************************
200   wa_mag = wa_magn(wa_static_mag, period)
c ***************************************
c *** bail out if sysmag is too small ***
c ***************************************
      if (sysmag.lt.0.00001) then
        write (punt,'('' sysmag.lt.0.00001 for '',a4)') msta(1:4)
   	sysmag = blank
        clusei = -1.0
        return
      endif
c *************************
c * compute ground motion *
c *************************
      gndmot = (amplit*10**3)/sysmag
c **********************************
c * find the correct distance term *
c **********************************
      do 320 i=1, max_loga0pts-1
        j = i + 1
        if (km.eq.loga0_tbl(1,i)) then
          loga0 = loga0_tbl(2,i)
          goto 400
        else if (km.eq.loga0_tbl(1,j)) then
          loga0 = loga0_tbl(2,j)
          goto 400
        else if (km.gt.loga0_tbl(1,i) .and. km.lt.loga0_tbl(1,j)) then
c ---     interpolate
          x1 = log10(loga0_tbl(1,i))
          x2 = log10(loga0_tbl(1,j))
          delta = x2 - x1
          if (abs(delta) .le. 0.0000001) then
            loga0 = loga0_tbl(2,i)
          else
            slope = (loga0_tbl(2,j) - loga0_tbl(2,i)) / delta
            loga0 = slope * (log10(km) - x1) + loga0_tbl(2,i)
          endif
          goto 400
        end if
320   continue
c --- we shouldn't be able to get here since
c --- outlaying distances were trapped above.
      clusei = -1
      return
c *****************************
c *** compute the magnitude ***
c *****************************
400   loga = log10( amplit * wa_mag / (2. * sysmag) )
c     loga0 = zmc1 - pwc1 * log10(km)
c     if (km.ge.40.) loga0 = zmc2 - pwc2 * log10(km)
      magnit = loga + loga0
c     write (punt,*) ' amp=',amplit,' period=',period,
c    &  ' dist_km=',km,' sysmag=',sysmag,' wa_mag=',wa_mag,
c    &  ' loga=',loga,' loga0=',loga0,' mag=',magnit
      clusei = sysmag / 1000000.
c *****************************************************
c *** exclude magnitude if distance is out of range ***
c *****************************************************
      if (km.lt.loga0_tbl(1,1) .or.
     &    km.gt.loga0_tbl(1,max_loga0pts)) then
        clusei = -1
      end if
      return
      end
c *************************************************************
c *************************************************************
c* (vax
c* (pc
c      subroutine get_uacaldata (filename, max_records, max_points,
c     &             nrecords, station_list, amp_source, begin_date,
c     &             end_date, npoints, period_sysmag, data_unit,punt)
cc           - subroutine get_uacaldata is not used on unix systems.
cc             however, please leave code here so that switch to vax
cc             can be made.
c* pc)
c* vax)
c* (pc
c$notruncate
c* pc)
c* (vax
c* (pc
cc --- input for this routine is a free formatted ascii file of
cc --- station calibration data.  it is a sequence of up to
cc --- <max_records> (logical) records (which may span several
cc --- physical records) ordered in the following manner:
cc --- 'sta_code', 'source', begin_yymmdd, end_yymmdd, npairs,
cc --- period(1), sysmag(1), period(2), sysmag(2), ...
cc --- period(npairs), sysmag(npairs)
cc --- note: the period, magnification pairs must be in order of
cc --- increasing period (i.e. this is reversed from the old routine).
c      character*300 calrec
c      character rshft*4, dnstrg*4
c      integer max_records, max_points, nrecords
c      character filename*(*), station_list(max_records)*(*)
c      character amp_source(max_records)*1
c      integer  begin_date(*), end_date(*), npoints(*), data_unit
c      real period_sysmag(2,max_points,max_records)
c      integer i, punt
c      call openfl(data_unit, filename, 'old', 'null', 'none',
c     *    'none', 0)
c      nrecords = 0
c100   if (nrecords.lt.max_records) then
c        nrecords = nrecords + 1
c        read (data_unit, '(a)', end=116) calrec
cc because standard fortran 77 to can not read an internal file with
cc free format, file 14 must be used in the following code!
c        rewind 14
c        write(14, '(a)') calrec
c        station_list(nrecords) = calrec(1:4)
c        rewind 14
c        if(calrec(4:4) .eq. ' ') then
c          amp_source(nrecords) = dnstrg(calrec(5:5))
c          write(14, '(a)') calrec(6:lentru(calrec))
c        else
c          amp_source(nrecords) = dnstrg(calrec(6:6))
c          write(14, '(a)') calrec(7:lentru(calrec))
c        endif
c        rewind 14
c        read (14, *, err=118)
c     &    begin_date(nrecords), end_date(nrecords), npoints(nrecords),
c     &    (period_sysmag(1,i,nrecords), period_sysmag(2,i,nrecords),
c     &    i=1, npoints(nrecords))
cc ---   this doesn't solve the overflow problem, but it might help.
c        if (npoints(nrecords).gt.max_points) then
c          npoints(nrecords) = max_points
c          write (*, '('' warning: too many data points on record #'',
c     &      i6, '' of uofa calibration file:'', /, a)')
c     &      nrecords, filename
c        endif
cc ---   right justify the 3-character station codes and shift to uppercase.
c        station_list(nrecords) = dnstrg(rshft(station_list(nrecords)))
cc ---   read next record
c        goto 100			
c116     continue
c          nrecords = nrecords - 1	
c          return
c118     continue
c          write (punt, 120) nrecords, filename
c120       format(' warning: error reading record #', i5,
c     &           ' from uofa calibration file:', /, 1x, a, /,
c     &           ' remainder of file skipped.')
c          return
c      else
c        write (punt, 130) max_records, filename
c130     format(' warning: too many records (>',i5,
c     &    ') in uofa calibration file:', /, 1x, a)
c      endif
c      return
c      end
c* pc)
c* vax)
cc *************************************************************
      real function log_interp (x, x1, y1, x2, y2)
c* (pc
c$notruncate
c* pc)
c --- returns the y cooresponding to a linear interpolation
c --- between x1, y1 and x2, y2 in the log-log plane.
c --- note that -1.0 is returned if any of the arguments are
c --- not positive. y1 is returned if the difference in the
c --- logs of x1 and x2 is too near zero.
      real x, x1, x2, y1, y2
      real log_x, log_x1, log_x2, log_y1, log_y2
      real delta, slope
      if ((x.le.0.0) .or.
     &    (x1.le.0.0) .or. (x2.le.0.0) .or.
     &    (y1.le.0.0) .or. (y2.le.0.0) ) then
c       write (punt,*) ' error', x, x1, y1, x2, y2
c ---   error, return error status
        log_interp = -1.0	
      else
        log_x = log10(x)
        log_x1 = log10(x1)
        log_x2 = log10(x2)
        log_y1 = log10(y1)
        log_y2 = log10(y2)
        delta = log_x2 - log_x1
        if (abs(delta) .le. 0.0000001) then
c ---     avoid dividing by zero
          log_interp = y1	
        else
          slope = (log_y2 - log_y1) / delta
          log_interp = 10.0 ** (slope * (log_x - log_x1) + log_y1)
        endif
      endif
      return
      end
c end uamag
c unfold2.for    []
      subroutine unfold2(alat,alon,la,ins,ala,lo,iew,alo)
c     unfold2
c
c-------- given geocentric lat and lon compute geographic coordinates
c            suitable  for printing
c
c input and output definition just reverse of entry fold2
c
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
      parameter (c1 = (1.0 - flat)**2)
      parameter (c2 = halfpi*(1.0/c1 - 1.0))
c
      character*1 ins, iew
c
c
c convert from geocentric lat and lon to geographic
c
c     alat     geocentric lat (radians)
c     alon                lon (radians)
c     la,ins,ala          lat in deg and min
c     lo,iew,alo          lon in deg and min
c
c gctogg - convert from geocentric to geographic latitude
      if (halfpi-abs(alat) .ge. 0.02) goto 403
         blat = c1*(alat + sign(c2,alat))
         goto 404
  403    blat = atan(tan(alat)/c1)
  404 continue
      ins = 'n'
      iew = 'w'
      ala1 = blat*deg
      la = ala1
      ala = abs(ala1 - la)*60.
      la = iabs(la)
      if (ala1 .lt. 0.0) ins = 's'
      alo1 = alon*deg
      lo = alo1
      alo = abs(alo1 - lo)*60.
      lo = iabs(lo)
      if (alo1 .lt. 0.0) iew = 'e'
      return
      end
c end unfold2
c update.for    []
      subroutine update(indexs, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr, ns,
     *         exdly, icent2, kdate, ihrmn,
     *         infil, iofil)
c given a current kdate and ihrmn, revise the station list arrays
c for all stations
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (irecsz = 132)
      real lat,lon
      dimension ielv(nsn),nsta(nsn)
      dimension lat(nsn),lon(nsn)
      dimension c(nsn),e(nsn),sw(nsn),ndate(nsn),nhr(nsn),klas(5, nsn)
      dimension calr(5, nsn),fmgc(nsn),xmgc(nsn),ipcod(nsn),iscod(nsn)
      integer fmwt, xmwt
      dimension fmwt(nsn),xmwt(nsn),tpdly(2,nsn)
      character*1 exdly(4, nsn), revp(6, nsn)
      character icard*(irecsz), dnstrg*(irecsz)
      character*4 nsta*5, nsta5*5
      character*1 ins, iew
      integer punt
      common /punt/ punt
cd    write(punt,5001) infil, iofil
cd5001 format(' sub update', 2i5)
      ihr = ihrmn/100
      rewind infil
      rewind iofil
      insv = infil
      infil = iofil
      iofil = insv
      skrd = 0
      in1 = 0
      l = 0
      kfnd = 0
cd    write(punt, '(a)') ' loop to find expired stations'
  200 l = l + 1
      if (l .gt. ns) goto 940
      call diftim(icent2, kdate,ihr,ndate(l),nhr(l),iporm,difhr)
      if (difhr .le. 0.) goto 200
  350 continue
cd    write(punt, '(a)') ' stat. l expired, so loop for current parameters'
      if (skrd .eq. 1) goto 432
      if ((in1 .eq. 0) .and. (indexs .gt. 0))
     *  write(punt,420) icent2,kdate,ihr
  420 format(' stations must be updated ', /,
     * ' date and time of next event:  ',i2,i6.6,1x,i2)
c
c main loop begins here ***************************************
c
  400 read(infil,425) icard
  425 format(a)
      icard = dnstrg(icard)
      nsta5 = icard(1:5)
      read(icard, 426) icymd, ih
  426 format(37x, i8, i2)
c     write(punt,5000) nsta5,icymd,ih,icard
c 5000 format(1x,a5,1x,i6,1x,i4,/,1x,a)
      if (nsta5(1:4) .eq. ' end') then
        write(iofil, '(a)') icard
        rewind infil
        rewind iofil
        insv = infil
        infil = iofil
        iofil = insv
        goto 800
      endif
c     write(punt, '(4a)') 'compare ', nsta5, ' with ', nsta(l)
      if ((kfnd .eq. 1) .and. (nsta5 .ne. nsta(l))) goto 750
  432 skrd = 0
      if (nsta5 .ne. nsta(l)) goto 480
cd    write(punt, '(a)') ' found correct station'
      kfnd = 1
      if (icymd .eq. 0) icymd = 99999999
      if (icymd .eq. 0) ih = 99
      if (icymd .lt. icent2*1000000+kdate) goto 400
      if ((icymd .eq. icent2*1000000+kdate) .and.
     *   (ih .lt. ihr)) goto 400
cd    write(punt, '(a)') ' found current parameters, so update information'
      call rdtmdp(l, icard, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr,
     *         exdly)
      if ((in1 .eq. 0) .and. (indexs .gt. 0)) write(punt,450)
  450 format(' name  stawt sys calr xmgc mgwt fmgc wght tl- code ',
     * 'alt-  pol   expiration   latitude    longitude  elev',/,
     *       '                                      p s dly      ',
     * 'dly        cnyrmody hr')
      in1 = 1
      if (indexs .gt. 0) then
        call unfold2(lat(l),lon(l),llat,ins,alat,llon,iew,alon)
        write(punt,475) nsta(l),  sw(l), klas(1, l),calr(1, l),
     *  xmgc(l), fmwt(l), xmwt(l), fmgc(l), ipcod(l), iscod(l), 
     *  tpdly(1,l),
     *  (exdly(i, l), i = 1, 4), tpdly(2,l), (revp(i,l), i = 1, 6),
     *  ndate(l), nhr(l),
     *  llat,   ins, alat, llon,   iew, alon, ielv(l)
  475   format(       1x,a5, 1x,f5.2,        i3,    1x,f5.2,
     *     f5.2,      i3,     i2,     f5.2,   1x,i2,       i2, 
     *        f5.2,
     *                      4a1,       f5.2,                   6a1,
     *        i9,       i3,
     *    i4, 1x,a1, f6.2,   i5, 1x,a1, f6.2,      i5)
      endif
      kfnd = 0
      goto 200
  480 continue
cd    write(punt, '(a)') ' write out - wrong station'
      write(iofil, '(a)') icard
      goto 400
  750 continue
cd    write(punt, '(a)') ' station has expired'
      skrd = 1
  800 if (indexs .gt. 0) write(punt,850) l, nsta(l)
  850 format(' the ',i4,'th station:',1x,a5,' has expired.')
      ndate(l) = 99999998
      kfnd = 0
      goto 200
  940 continue
cd    write(punt, '(a)') ' put rest of infil on iofil'
      if (skrd .eq. 1) goto 975
  950 read(infil, '(a)', end=1000) icard
  975 write(iofil, '(a)') icard
      goto 950
 1000 return
      end
c end update
c upstrg.for    []
      character*(*) function upstrg(array)
c
c  program to change string to uppercase
c
c  array - a character variable
      character*(*) array
      integer offset
      data offset/32/
c
c  get length of array
c
      upstrg = ' '
      lenstr = len(array)
      if (lenstr .eq. 0) return
      do 10 i = 1, lenstr
        ic = ichar(array(i:i))
        if ((ic .ge. 97) .and. (ic .le. 122)) then
          upstrg(i:i) = char(ic - offset)
        else
          upstrg(i:i) = array(i:i)
        endif
   10 continue
      return
      end
c end upstrg
c upward.for    []
       subroutine upward(alrms, altla, altlo, altz,
     * frms, rmslim, zup, axz)
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /phoqn/ inst,knst
      real lonep, latep
      common /qmost1/ lonep,ni,latep
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      dimension zinc(8)
      data zinc/20, 20, 20, 20, 20, 20, 20, 20/
c set initial steps according to erz estimate
      zbas = axz
      if(zbas .lt. .2) zbas = .2
      if(zbas .gt. 20.) zbas = 20.
      zinc(1) = zbas
      zinc(2) = zbas*2.
      inst = 1
c find upward shift in z with respect to altz that results in rms = rmslim
        if(frms .le. rmslim) then
c the rms at the surface is within error limits
          zup = altz
        else if(altz .le. 5.) then
c depth is near surface and
c the rms at the surface is higher than error limit, so interpolate.
          zup = altz*(rmslim - alrms)/(frms - alrms)
        else
          savla = altla
          savlo = altlo
          if( (altz - zinc(1)) .lt. 0.0 ) zinc(1) = 0.7*altz
          savez = altz - zinc(1)
          alrmsl = alrms
          altzl = altz
          n = 2
20        call quakes
          if(rms .ge. rmslim) then
c interpolate to get zup
            zup = altz - altzl + (altzl - savez)*(rmslim - alrmsl) /
     *            (rms - alrmsl)
          else
            if(savez .gt. zinc(n)) then
              altzl = savez
              savez = savez - zinc(n)
              n = n + 1
              if(altz - savez .ge. 110.) then
                zup = 99.
                return
              endif
              alrmsl = rms
              savla = latep
              savlo = lonep
              goto 20
            else
c interpolate to get zup based on surface rms
              zup = altz - savez + savez*(rmslim - rms) /
     *        (frms - rms)
            endif
          endif
        endif
      return
      end
c end upward
c usedly.for    []
      subroutine usedly(k, zuse, modset, alat, alon)
c
c this sub is set up by each user to make such parameters as
c    delay model (kno), crustal model (modset), starting
c    depth maximum (test36)), and (or) starting depth (zuse)
c    a function of eq location.
c
c     integer   kno
c               kno     output - delay model to use on next iteration
c
      integer   k
c               k       input - index of station for which travel time
c                               will be computed next, unless called
c                               from hymain, in which case k = 0.
      real      zuse
c               zuse    output - set to 99999. of starting depth is not
c                                being set by this sub.
      integer   modset
c               modset  output - next crustal model to be used.  if zero,
c                                then use model preferred by each station.
      real      px(6), py(6)
c     real      px(6), py(6)     x and y values defining the gulf of alaska
c
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      logical ingulf
      real lat, lon
      character*1 bksrc
      integer punt
      common /punt/ punt
      common /dipu/ ipkdly, igsum, bksrc
      common /gu/ ingulf
      common /hopq/ savla,savlo,savez,savor,freor
      common /iclmpq/ lat(nsn),lon(nsn)
      common /ihfgpq/ itest(100)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /imost/ test(100)
      common /logfil/ logfil
      character mtyp*2
      common /iu/ mtyp(mmax)
c if instset .ne. ' ', then put this new value of inst on summary record
      character*1 instset
      common /hnu/ instset
      common /phoqn/ inst,knst
      common /qmost/ wt(npa),z
      common /qmost1/ lonep,ni,latep
      character*1 ins, iew
      data px/-436.41858,  56.64243, 225.09338, 469.42575,
     * 573.36530, 745.52167/
      data py/-533.93683,-166.69151,-138.00677,-283.51489,
     *-303.27237,-489.55786/
cd    if(k .eq. 0) then
cd      print *, 'usedly called from hymain with k = 0'
cd    else
cd      print *, 'index of station for which travel time delay'
cd      print *, 'needs to be computed is ', k
cd    endif
c zuse of 99999. will be ignored in setting the starting depth.
      zuse = 99999.
      if (ipkdly .gt. 0) then
c select delay model basesd on closest station
        if(k .eq. 0) then
          kno = ipdly(1)
        else
          kno = ipdly(k)
        endif
c       crustal model will be set to modset, unless modset = zero.
c       in which case the model preferred by each station will be used.
        modset = 0
        return
      else
c convert eq location to geographic coordinates> bla, blo
        call unfold2(alat, alon, la, ins, ala, lo, iew, alo)
        bla = la + ala/60.
        if(ins .eq. 's') bla = -bla
        blo = lo + alo/60.
        if (iew .eq. 'e') blo = -blo
      endif
cd    print *, 'eq location is (geographic) ', bla, blo
cd    print *, 'ipkdly = ', ipkdly
      if (ipkdly .eq. 0) then
c **************************************************
c this is the old option for selecting delay and velocity models
c **************************************************
        ingulf = .false.
        if ((bla .lt. 59.7) .and. (blo .lt. 154.)
     *  .and. (blo .gt. 135.)) then
cd         print *, 'roughly in gulf of alaska'
          if ((bla .lt. 57.9) .and. (blo .lt. 146.)
     *    .and. (blo .gt. 137.3)) then
cd           print *, 'definately in gulf of alaska'
            ingulf = .true.
          else
            call delaz(lat(1), lon(1), delt, deldeg, azz, alat,alon)
cd           print *, 'reference station (geocentric) ', lat(1), lon(1)
cd           print *, 'location of epicenter wrt reference station is'
cd           print *, 'azimuth (deg) = ', azz
cd           print *, 'distance (km) = ', delt
            x0 = delt*sin(azz*rad)
            y0 = delt*cos(azz*rad)
            ini = inside(x0, y0, px, py, 6)
            if (iabs(ini) .eq. 2) ingulf = .true.
          endif
        endif
        if (ingulf) then
          if((inst .eq. 0) .or. (inst .eq. 8))then
            kno = 5
            modset = 5
            inst = 1
            instset = '1'
            savez = 13.
            zuse = 13.
            z = 13.
            test(36) = 13.
          else
            kno = 5
            modset = 5
            test(36) = 13.
          endif
          return
        else if (blo .gt. 148) then
c western
          kno = 1
          test(36) = 100.
        else if (blo .gt. 144.5) then
c central
          kno = 2
          test(36) = 45.
        else if (((blo .le. 142.25) .and. (blo .ge. 138.)) .and.
     *     (bla .lt. 61.)) then
c    *     ((bla .lt. 61.) .and. (bla .gt. 59.25))) then
c icy bay
          kno = 3
          test(36) = 25.
        else
c eastern, except for icy bay
          kno = 4
          modset = 4
          test(36) = 25.
        endif
      else if (ipkdly .lt. 0) then
c ***********************************************************
c this is the new option for use of cylindrical delay regions
c ***********************************************************
c if iteration number is .gt. test(37) then do not change
c crustal model number or delays.  this is to prevent boundary
c chatter.
        if(ni .gt. itest(37)) return
c ***********************************************************
c
c       cyldly computes delay model ndly, based on the current eq
c       location and the cylindrical delay domains.
        call cyldly
     *  (kno, alat, alon, lat(1), lon(1), x0, y0, z, dly, sdly, iprn,
     *   modset, test(8))
c	   if cyldly returns modset .ne. 0, then it will not be 
c	    reset below, except for gulf of alaska.
c          kno=1 is the default delay model
c	   kno=2,3, and 4 are the volcanic axis delay models
c	   kno=5 is the gulf of alaska delay model
c	   kno is set to 11 if a combined delay model is computed.
cd       print *, 'reference station (geocentric) ', lat(1), lon(1)
cd       print *, 'location of epicenter wrt reference station is'
cd       print *, 'x0, y0 = ', x0, y0
cd       print *, 'next select velocity model based on eq location'
        ingulf = .false.
        if ((bla .lt. 59.7) .and. (blo .lt. 154.)
     *  .and. (blo .gt. 135.)) then
cd         print *, 'roughly in gulf of alaska'
          if ((bla .lt. 57.9) .and. (blo .lt. 146.)
     *    .and. (blo .gt. 137.3)) then
cd           print *, 'definately in gulf of alaska'
            ingulf = .true.
          else
            ini = inside(x0, y0, px, py, 6)
            if (iabs(ini) .eq. 2) then
cd             print *, 'in gulf of alaska, based on sub. inside'
              ingulf = .true.
              if(iprn .ge. 5) write (punt, *) ' in gulf'
            endif
          endif
        endif
        if (ingulf) then
c         this overrides the cylinder specification
	  kno = 5
          if((inst .eq. 0) .or. (inst .eq. 8))then
            modset = 1
            inst = 1
            instset = '1'
            savez = 13.
            zuse = 13.
            z = 13.
            test(36) = 13.
          else
            modset = 1
            test(36) = 13.
          endif
          return
        else if (bla .lt. 62.5) then
cd         print *, 'southern region'
           if(iprn .ge. 5) write (punt, *) ' in southern region'
          if(modset .eq. 0) modset = 2
          test(36) = 100.
        else
cd         print *, 'northern region'
           if(iprn .ge. 5) write (punt, *) ' in northern region'
          if(modset .eq. 0) modset = 3
          test(36) = 50.
        endif
      endif
      if (modset .le. kl) return
cd     print *, 'modset, kl ', modset, kl
      if ( (modset .lt. 11) .or. (modset .gt. 13) ) then
        write(logfil, 80) modset
        write(punt, 80) modset
80      format (' usedly may not set the crustal model to ', i5,
     *          ', because that model has not been defined.', /,
     *          ' xxxx stop xxxx')
        stop
      endif
      return
      end
c end usedly
c velazm.for    []
      subroutine velazm
c find azimuth, apparent velocity, and residuals for plane wave
c    traveling across network form a distant event.
      include 'params.inc' 
      parameter (ndly = 11)
      double precision pp,q,r,s,t,u,v,y
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /gmost/ az(npa)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /ilotv/ elvdly(npa)
      common /ilv/ c(nsn),e(nsn)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pgnoqv/ p(npa),jmin(npa)
      common /pgnov/ dt(npa)
      common /pgqv/ w(npa)
      character*4 krmp
      common /pnoqv/ krmp(npa)
      common /povx/ amx(npa)
      common /qmost/ wt(npa),z
      common /tmost/ x(4,npa)
      dimension tim(npa),tx(npa)
      nrpp=0
      do 8 i=1,nrp
        if (w(i).ne.0.0) nrpp=nrpp+1
    8 continue
      if (nrpp.gt.3) goto 10
      write(punt,9) nrpp
    9 format(/, ' insufficient data to calculate azimuth and apparent',
     * ' velocity.', /,
     * ' only ', i3, ' p-wave readings with weights not equal to zero.')
      return
   10 kno=1
      pp=0.0d00
      q=0.0d00
      r=0.0d00
      s=0.0d00
      t=0.0d00
      d=0.0
      u=0.0d00
      v=0.0d00
      y=0.0d00
c normalize reading weights.  use no other weights.
      sumw=0.0
      do 15 i=1,nrp
        wt(i)=w(i)
        sumw=sumw+w(i)
   15 continue
      sumw=sumw/nrp
c set up 3x3 matrix by multiplying through by transpose.
      do 20 i=1,nrp
        wt(i)=wt(i)/sumw
        ji=kdx(i)
        tim(i)=tp(i)-dly(kno,ji)-elvdly(i)
        pp=pp+c(ji)*c(ji)*wt(i)
        q=q+e(ji)*e(ji)*wt(i)
        r=r+c(ji)*e(ji)*wt(i)
        s=s+tim(i)*c(ji)*wt(i)
        t=t+tim(i)*e(ji)*wt(i)
        d=d+tim(i)*wt(i)
        u=u+c(ji)*wt(i)
        v=v+e(ji)*wt(i)
        y=y+wt(i)
   20 continue
c solve 3x3 by matrix equation
      alp1=pp*v-r*u
      bet1=r*v-q*u
      gam1=t*u-s*v
      alp2=r*y-u*v
      bet2=q*y-v*v
      gam2=d*v-t*y
      a=(gam1*bet2-gam2*bet1)/(alp2*bet1-alp1*bet2)
      b=-(gam1+alp1*a)/bet1
      cc=( s-a*pp-b*r)/u
      sum=0.0
      sumw=0.0
      btt=0.0
      if ( iprn.lt. 3) goto 25
      write(punt,21)
   21 format(51h velazm format 21. matrix equation a(3 by 3) x b(3),
     * 8h = c(3)./)
      write(punt,22) pp,r,u,a,s,r,q,v,b,t,u,v,y,cc,d
   22 format(3(5x,3f10.3,2(9x,f10.3),/))
   25 do 30 i=1,nrp
        ji=kdx(i)
        tx(i)=a*c(ji)+b*e(ji)+cc
        x(4,i)=tim(i)-tx(i)
        tx(i)=tx(i)-dt(i)
        if ( kdx(i).eq.1) btt=x(4,i)
        sumw=sumw+abs(x(4,i))*wt(i)
        sum=sum+(x(4,i)**2)*wt(i)
   30 continue
c calculate apparent velocity, standard deviation, and rms.
      vapp=1./sqrt(a**2+b**2)
      yy=y
      xmean=sumw/yy
      rms=sqrt(sum/yy)
      sd =sqrt(sum/(yy-3.0))
c determine azimuth being careful to pick the right quadrant
      if ((a.ne.0.0).or.(b.ne.0.0)) goto 300
      angn=999.
      goto 310
  300 angn=atan2( a,b)*57.29578
      if (angn.lt.0.0) angn=360.0+angn
  310 iang=angn+0.5
      jang=iang+180
      if (jang.ge.360) jang=jang-360
      kmin=jmin(1)
c write the output on the printer
      write(punt,500) kdate,khr,kmin,iang,jang,vapp,xmean,rms,sd
  500 format(/,
     * ' plane wave solution.  yrmody hr:min = ', i8, 1x, i2,':',i2, /,
     * ' azimuth of approach    = ', i5,
     * ' degrees clockwise from north.', /,
     * ' azimuth to source      = ', i5, ' degrees', /,
     * ' apparent velocity      = ', f6.2, ' km/sec', /,
     * ' mean of abs residuals  = ', f6.2, ' sec', /,
     * ' rms of the residuals   = ', f6.2, ' sec', /,
     * ' standard deviation     = ', f6.2, ' sec')
      write(punt,499) nsta(1)
  499 format(/, ' the residuals are relative to station ',a5,//,
     * '  sta   prmk  hrmn  p-sec  p-cal  dly/h1 elvdly p-res rel-',
     * 'res weight amax   dt',/)
      do 505 i=1,nrp
        ji=kdx(i)
        iamx=amx(i)+0.5
        rres=x(4,i)-btt
        write(punt,502)msta(i),krmp(i),khr,jmin(i),p(i),tx(i),
     *  dly(kno,ji),elvdly(i),x(4,i),rres,wt(i),iamx,dt(i)
  502   format(1x,a5,2x,a4,2x,2i2,7f7.2,i5,f7.2,f10.2)
  505 continue
      return
      end
c end velazm
c wadati.for    []
      subroutine wadati(nr, nrp, w, ldx, tp, kdate, khrmn, iprn,
     *      ilis, krmp, krms, msta, test, vpvs3,  worig, se3)
c               original version written by w. gawthrop 197?.
c               modified and comments added by c. stephens, aug 1981.
c               modified for addition to hypoellipse and to account
c               for estimated reading errors by j. c. lahr, jan 1986.
c-------------------------------------------------------------------
c   this program is used to generate wadati (s-p interval vs
c   p-arrival time) plots on the line printer from hypoellipse phase
c   data.  the program determines the vp/vs ratio (slope of the ts
c   vs tp curve), the origin time (tp intercept) and standard error
c   of the slope.
c   the phase data from one or more earthquakes may be input.  a
c   separate plot will be made and parameters determined for each
c   event, and an average vp/vs for all of the events will be
c   determined.
c
c-------------------------------------------------------------------
      include 'params.inc' 
      character*4 isym(npa), msta(npa)*5
      character*4 krmp(npa), krms(npa)
      dimension w(npa), ldx(npa), tp(npa), key(npa)
      dimension test(100)
      dimension par(npa), pwt(npa), sar(npa), swt(npa)
      integer punt
      common /punt/ punt
c------- check for s readings
      if((nr-nrp) .ge. 3) goto 31
29      if(iprn .ge. 1 .and. ilis .gt. 0) write(punt, 30) nr, nrp
30      format(' too few s readings for wadati calculation:', /,
     *         '  of ', i5, ' readings, ', i5, ' are p.')
        vpvs3 = 0.
        return
31    continue
      pimax = 0.
c------- main loop to compute vp/vs and origin time
c assume s is dependent and p is independent variable
      nvpvs = 0
      do 40 i=1,nrp
c------- determine station plot symbol
c------- no s data
      if(ldx(i) .eq. 0) goto 40
c------- s weight = 0
      if(w(ldx(i)) .eq. 0.) goto 40
c------- p weight = 0
      if(w(i) .eq. 0.) goto 40
c------- found one with p and s data
      nvpvs = nvpvs + 1
      key(nvpvs) = i
      isym(nvpvs) = msta(i)(1:4)
      par(nvpvs) = tp(i)
      pwt(nvpvs) = w(i)
      if(par(nvpvs) .gt. pimax) pimax = par(nvpvs)
      sar(nvpvs) = tp(ldx(i))
      swt(nvpvs) = w(ldx(i))
40    continue
      if(nvpvs .lt. 3) goto 29
      call line3(nvpvs, par, sar, pwt, swt, test,
     *                         vpvs1, sint1, orig1, se1, ses, oses,
     *                         vpvs2, sint2, orig2, se2, sep, osep,
     *                         vpvs3, sint3, orig3, se3)
      khrmn1 = khrmn
      if(orig1 .lt. 0.) then
        khrmn1 = khrmn1 - 1
        orig1 = orig1 + 60.
      endif
      khrmn2 = khrmn
      if(orig2 .lt. 0.) then
        khrmn2 = khrmn2 - 1
        orig2 = orig2 + 60.
      endif
      khrmn3 = khrmn
      worig = orig3
      if(orig3 .lt. 0.) then
        khrmn3 = khrmn3 - 1
        orig3 = orig3 + 60.
      endif
      if(iprn .le. 0 .and. ilis .gt. 0) 
     *  write(punt, 41) vpvs3, se3, nvpvs
41    format (' bi-weight vp/vs = ', f8.3, ' +/- ', f5.3,
     * ' based on ', i5, ' stations with p and s')
      if(iprn .ge. 1 .and. ilis .gt. 0) write(punt, 42)
     *  vpvs1, se1, kdate, khrmn1, orig1, sint1, oses, ses,
     *  vpvs2, se2, kdate, khrmn2, orig2, sint2, osep, sep,
     *  vpvs3, se3, kdate, khrmn3, orig3, sint3
42    format (/,
     * '              vp/vs ratio    std. error of vp/vs  date ',
     *'  hrmn  origin   s-intercept  estimated std. er.  std. er. used',
     * /,
     * ' 1) s vs p   ', f12.4, f17.4, i13, i6, f8.2, f12.2, 8x,
     *                     f5.2, 13x, f5.2, /,
     * ' 2) p vs s   ', f12.4, f17.4, i13, i6, f8.2, f12.2, 8x,
     *                     f5.2, 13x, f5.2, /,
     * ' 3) bi-weight', f12.4, f17.4, i13, i6, f8.2, f12.2)
c------- write out phase data used in wadati calculation
      if(iprn .ge. 1 .and. ilis .gt. 0) write(punt, 20)
20    format (/,
     *' station  prmk  p-time    p-wt  srmk  s-time    s-wt  deviation',
     *' from line')
c      xxxaaaaxxxaaaaxxffffffxxffffffxxaaaaxxffffffxxffffff
      denom = sqrt(1. + vpvs3*vpvs3)
      do 60 i = 1, nvpvs
        j = key(i)
        dev = (sar(i) - sint3 - vpvs3*par(i))/denom
        if(iprn .ge. 1 .and. ilis .gt. 0)
     *   write(punt, 35) isym(i), krmp(j), par(i), pwt(i),
     *   krms(j), sar(i), swt(i), dev
35      format (      3x,a4, 3x,a4, 2x,f6.2, 2x,f6.2, 2x,a4, 2x,f6.2,
     *                  2x,f6.2, 9x,f6.2 )
60    continue
      if(abs(test(49)) .gt. 1.) then
c------- extend array for plotting fit lines
        do 62 i = 1, nvpvs
          par(i) = par(i) - worig
          sar(i) = sar(i) - worig
62      continue
        xaxmx = pimax - worig + 2.
        yaxmx = xaxmx*vpvs3
        par(nvpvs+1) = xaxmx - 1.
63      sar(nvpvs+1) = vpvs3*par(nvpvs+1)
        if(sar(nvpvs+1) .gt. yaxmx) then
          par(nvpvs+1) = par(nvpvs+1) - 1.
          goto 63
        endif
        isym(nvpvs+1) = '3'
        par(nvpvs+2) = 1.1
        sar(nvpvs+2) = vpvs3*par(nvpvs+2)
        isym(nvpvs+2) = '3'
        par(nvpvs+3) = xaxmx - 1.5
64      sar(nvpvs+3) = sint1 + worig*(vpvs1 - 1.) +
     *    vpvs1*par(nvpvs+3)
        if(sar(nvpvs+3) .gt. yaxmx) then
          par(nvpvs+3) = par(nvpvs+3) - 1.
          goto 64
        endif
        isym(nvpvs+3) = '1'
        par(nvpvs+4) = .8
        sar(nvpvs+4) = sint1 + worig*(vpvs1 - 1.) +
     *    vpvs1*par(nvpvs+4)
        isym(nvpvs+4) = '1'
        par(nvpvs+5) = xaxmx - 2.5
66      sar(nvpvs+5) = sint2 + worig*(vpvs2 - 1.) +
     *    vpvs2*par(nvpvs+5)
        if(sar(nvpvs+5) .gt. yaxmx) then
          par(nvpvs+5) = par(nvpvs+5) - 1.
          goto 66
        endif
        isym(nvpvs+5) = '2'
        par(nvpvs+6) = .5
        sar(nvpvs+6) = sint2 + worig*(vpvs2 - 1.) +
     *    vpvs2*par(nvpvs+6)
        isym(nvpvs+6) = '2'
        kk = nvpvs + 6
        if(iprn .ge. 1 .and. ilis .gt. 0) write(punt, 70) nvpvs
70      format(1x, 'wadati plot with ', i5, ' data points.')
c------- make wadati plot
cd      print *, 'par ', par
cd      print *, 'sar ', sar
cd      print *, 'isym ', isym
        if(iprn .ge. 0 .and. ilis .gt. 0)
     *  call prplot(par, sar, xaxmx,  0., yaxmx,  0.,   56,  kk,isym,
c                      x,  y,  xmax,xmin,  ymax,ymin,lines,last,isym,
     *     1,   1)
c         no,most)
      endif
      return
      end
c end wadati
c weight.for    []
      subroutine weight
      include 'params.inc' 
      parameter (ndly = 11)
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /anox/ keyd(npa)
      common /bqz/ avrps,avuse
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /ihfgpq/ itest(100)
      common /iiq/ wf(51)
      common /pgqv/ w(npa)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /phoqn/ inst,knst
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pnoqtx/ ldx(npa)
      common /qmost/ wt(npa),z
      common /qmost1/ lonep,ni,latep
      common /qgnotx/ delta(npa)
      common /qo/ dmax,ndec,adjsq,iph
      common /qw/ azmv(5), iuse(npa), duse(npa), ndwt, iboxc
      logical ddone, adone, tdone, bdone, jdone
      common /qw1/ ddone, adone, tdone, bdone, jdone
      common /rq/ xmean(4),avwt,aveaj
      common /tmost/ x(4,npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      data pi/3.1415927/
c----
c weight arrival times 5 different ways:
c------- nrwt is the total number of readings with weight not zero
c------- fno is the sum of p,s and s-p weights.
c------- nsmp is the total number of s-p data with weight not zero
c------- onf is the sum of p and s weights
c----
c distance weighting
      ndout = 0
      if((ni .ge. itest(10)) .and. (.not. ddone)) then
c compute distance weights this iteration
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 610)
610     format(' apply distance weighting')
        ddone = .true.
        dmax = test(12)
        if(itest(46) .ne. 0) then
c set xfar to 10 km beyond the distance of the test(46)th station
          n10 = iabs(itest(46) )
          if(ndwt .lt. n10) n10 = ndwt
          if(duse(n10) .gt. dmax) dmax = duse(n10) + 10.
        endif
620     xfn = dmax - test(11)
        do 631 i = 1,nr
          if(wt(i) .gt. 0.0) then
c since station has weight, check distance weight
            if(delta(i) .le. test(11)) then
c full weight
            else if(delta(i) .lt. dmax) then
c partial weight
c             wt(i) = wt(i)*((dmax - delta(i))/xfn)**2
              thet = pi*(delta(i) - test(11))/xfn
              distwt = .5*(cos(thet) + 1.0)
              wt(i) = wt(i)*distwt
            else
c no weight
              ndout = ndout + 1
              wt(i) = 0.0
              kwr(i) = 'd'
            endif
          endif
631     continue
c
c reweight stations that are distant but reduce a gap .gt. 100
c       by 50 degrees or more.
        if((ndout .gt. 0) .and. (itest(46) .lt. 0))
     *  call redgap(iuse, w, wt, kwr, az, keyd, ldx, nrp)
cd      print *, 'iuse w     wt    kwr  az    keyd i'
cd      print '(i5, f6.2, a1, i5, f6.2, 3i5)',
cd   *  (iuse(i), w(i), wt(i), kwr(i), az(i), keyd(i), ldx(i),
cd   *  i, i = 1, nrp)
        call zstats
      endif
c
c azimuthal weighting
      iazch = 0
      if((ni .ge. itest(13)) .and. (.not. adone)) then
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 634)
634     format(' apply azimuthal weighting')
        adone = .true.
        call azwtos
        call zstats
      endif
c
c truncation weighting
      if((ni .ge. itest(14)) .and. (.not. tdone)) then
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 635)
635     format(' apply truncation weighting')
        tdone = .true.
636     continue
        if(nrwt .ge. 6) then
          resmx = 0.0
          do 640 i = 1,nr
            if(wt(i) .eq. 0.0) goto 640
            dif = abs(x(4,i)-ksmp(i)*avuse)
            if(dif .le. resmx) goto 640
            resmx = dif
            iresx = i
640       continue
          if(resmx .le. test(15)) goto 650
          wt(iresx) = 0.0
          kwr(iresx) = 'm'
          call zstats
          goto 636
        endif
      endif
c
c boxcar weighting
650   nobox=0
      if((ni .eq. itest(16)) .and. (.not. bdone)) then
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 652)
652     format(' apply boxcar weighting')
        bdone = .true.
        if (nrwt .ge. 6) then
          t34 = rms
          if(rms .lt. abs(test(29))) t34 = abs(test(29))
          do 660 i = 1,nr
            if(wt(i) .eq. 0.0) goto 660
            dif = abs(x(4,i)-ksmp(i)*avuse)
            if(dif .gt. test(17)*t34) then
              kwr(i) = 'b'
              wt(i) = 0.0
              nobox=nobox+1
            endif
660       continue
          if(nobox .gt. 0) then
            call zstats
            goto 650
          endif
        endif
      endif
c
c jeffreys weighting
674   if( (ni .eq. itest(18)) .and. (rms .gt. test(19))
     * .and. (.not. jdone) ) then
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 675)
675     format(' apply jeffreys weighting')
        jdone = .true.
        t34 = rms
        if(t34 .lt. abs(test(29))) t34 = abs(test(29))
        do 690 i=1,nr
          if(wt(i) .eq. 0.0) goto 690
          k = 10.*abs((x(4,i)-ksmp(i)*avuse)/t34) + 1.5
          if(k .gt. 51) k = 51
          wt(i) = wt(i)*wf(k)
          if(k .gt. 30) kwr(i) = 'j'
690     continue
        call zstats
      endif
      call zstats
      return
      end
c end weight
c xfmags.for    []
      subroutine xfmags
c compute f-magnitude and x-magnitude
c added xmag correction to UofA magnitudes.  3/16/94  jcl
      include 'params.inc' 
      parameter (ndly = 11)
      real mag
      dimension xtemp(npa), ftemp(npa)
      character*4 iahead*60, msta*5, nsta*5, icard*110, uacal*50
      character*1 dnstrg
      common /char/ iahead, msta(npa), nsta(nsn), icard
      logical medmag 
      common /dinx/ imag,imagsv,medmag
      character*1 bksrc
      common /dipu/ ipkdly, igsum, bksrc
      common /dix/ iexcal, uacal
      common /dmost/ ipun,ivlr,blank
      common /ihfgpq/ itest(100)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /iox/ prr(nsn),iuses
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn), xmwt(nsn)
      common /ix/      ir,qspa(9,40)
      common /olnx/ xmag(npa),fmag(npa),avxm,avfm,xmmed,fmmed
      common /onx/ kluse(npa), cluse(npa), peruse(npa), ampuse(npa)
      common /ox/ sysmag(npa), gndmot(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      common /pnoqtx/ ldx(npa)
      common /povx/ amx(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /pox/ fmp(npa),prx(npa),icent1,icent2
      integer punt
      common /punt/ punt
      common /qmost/ wt(npa),z
      common /qgnotx/ delta(npa)
      common /tonxb/ t(npa),fms(npa)
      common /tmost/ x(4,npa)
      common /xfmno/ mag
      character*1 mgndx
      common /xfmno1/ mgndx
      common /xo/ nm,sdxm,nf,sdfm
      character*1 kodsym
      common /xo1/ kodsym(npa)
      character*1 ampsc
      dimension rspa(8,20)
      data zmc1,zmc2,pwc1,pwc2/0.15,3.38,0.80,1.50/
      data rspa/-0.02, 1.05, 0.60, 0.26, 0.66, 0.55, 0.17, 0.42,
     *           0.14, 1.18, 0.67, 0.23, 0.79, 0.66, 0.27, 0.64,
     *           0.30, 1.29, 0.74, 0.19, 0.90, 0.76, 0.35, 0.84,
     *           0.43, 1.40, 0.79, 0.12, 1.00, 0.86, 0.43, 0.95,
     *           0.55, 1.49, 0.84, 0.05, 1.08, 0.93, 0.49, 1.04,
     *           0.65, 1.57, 0.89,-0.03, 1.16, 1.00, 0.55, 1.13,
     *           0.74, 1.63, 0.94,-0.10, 1.23, 1.07, 0.63, 1.24,
     *           0.83, 1.70, 1.00,-0.16, 1.30, 1.15, 0.72, 1.40,
     *           0.92, 1.77, 1.06,-0.20, 1.38, 1.25, 0.83, 1.50,
     *           1.01, 1.86, 1.11,-0.24, 1.47, 1.35, 0.95, 1.62,
     *           1.11, 1.96, 1.14,-0.28, 1.57, 1.46, 1.08, 1.73,
     *           1.20, 2.05, 1.15,-0.33, 1.67, 1.56, 1.19, 1.84,
     *           1.30, 2.14, 1.14,-0.39, 1.77, 1.66, 1.30, 1.94,
     *           1.39, 2.24, 1.11,-0.46, 1.86, 1.76, 1.40, 2.04,
     *           1.47, 2.33, 1.06,-0.53, 1.95, 1.85, 1.50, 2.14,
     *           1.53, 2.41, 1.00,-0.62, 2.03, 1.93, 1.58, 2.24,
     *           1.56, 2.45, 0.92,-0.71, 2.07, 1.97, 1.62, 2.31,
     *           1.53, 2.44, 0.84,-0.80, 2.06, 1.96, 1.61, 2.31,
     *           1.43, 2.36, 0.75,-0.89, 1.98, 1.88, 1.53, 1.92,
     *           1.25, 2.18, 0.66,-0.99, 1.82, 1.72, 1.37, 1.49/
      mgndx = ' '
      mag = blank
      zsq = z**2
      nm = 0
      avxm = 0.0
      sdxm = 0.
      nf = 0
      anf = 0.0
      avfm = 0.0
      sdfm = 0.
c----
c start loop through each station
      if(iprn .ge. 5) write(punt, '(a)') 'begin xfmags'
      do 40 i = 1,nrp
c   xmag calculation
c 	print *, ' station = ', phcard(keyphi(i))(1:4)
        xmag(i) = blank
	sysmag(i) = blank
	gndmot(i) = blank
        rad2 = delta(i)**2 + zsq
        cluse(i) = -1.
c set period
        peruse(i) = prx(i)
        if (peruse(i) .lt. 0.01) peruse(i) = prr(kdx(i))
c      print *, ' rad2 = ', rad2
c       if ((rad2.lt.1.) .or. (rad2.gt.360000.)) goto 30
        amxi = abs(amx(i))
c      print *, 'amxi = ', amxi
        if (amxi.lt.0.01) goto 30
c       take care of a1vco gain range state
        if (phcard(keyphi(i))(62:62) .eq. '1') then
          ampuse(i) = amxi*10.
        else if (phcard(keyphi(i))(62:62) .eq. '2') then
          ampuse(i) = amxi*500.
        else
          ampuse(i) = amxi
        endif
c the amplitude source is phcard(i)(108:108)
        ampsc = phcard(keyphi(i))(108:108)
c      print *, 'amplitude source code = ', ampsc
        if(ampsc .eq. ' ') ampsc = bksrc
        ampsc = dnstrg(ampsc)
        if(ampsc .eq. 'j' .or. ampsc .eq. 'x') ampsc = 'd'
c      print *, 'amplitude source code = ', ampsc
c      print *, 'klas = ', klas(1, kdx(i))
        if (klas(1, kdx(i)) .eq. 18) then
c Use the masscomp calibration for PC data also.
	  if((ampsc .eq. 'p' .or.
     *	      ampsc .eq. 'o' .or.
     *	      ampsc .eq. 'u' .or.
     *	      ampsc .eq. 'i' .or.
     *	      ampsc .eq. 'g' .or.
     *	      ampsc .eq. 'q' .or.
     *	      ampsc .eq. 'k')) ampsc = 'd'
c Leave z alone, as this is entered in caldata.prm as z.  jcl 9/1/95
c         use university of alaska magnitude subroutine
          kluse(i) = klas(1, kdx(i))
c      print *, ' call uamag( msta ampsc kdate rad2 ampuse prx )'
c      print *,  msta(i), ampsc, kdate, rad2, ampuse(i), prx(i)
          call uamag (icent2, msta(i), ampsc, kdate, rad2, ampuse(i),
     &        prx(i), xmag(i), cluse(i), sysmag(i), gndmot(i),
     &        test(52), punt, blank)
          if (cluse(i) .eq. -1) then
	    xmag(i) = blank
	    goto 30
	  endif
	  xmag(i) = xmag(i) + xmgc(kdx(i))
c if xmgc is .ge. 6, then subtract 10 and do not
c include in average.
          if (xmgc(kdx(i)) .ge. 6.) then
            xmag(i) = xmag(i) - 10.
            goto 30
          endif
c         print *, 'xmag cluse = ', xmag(i), cluse(i)
        else
c all other klas values are processed here
c take care of semens playback setting
          if ((phcard(keyphi(i))(61:61) .eq. '1') .and.
     *      (ampsc .eq. 's')) then
            ampuse(i) = ampuse(i)*4.
          endif
          if ( (ampsc .eq. 'e') .or. (ampsc .eq. 'v') .or.
     *         (ampsc .eq. '1') .or. (ampsc .eq. '4') .or.
     *         (ampsc .eq. ' ') .or. (ampsc .eq. '*') ) then
c use standard usgs film calibration, system response in first position
c this is necessary because the usgs data does not distinguish between
c 20x film/202-vco data (response=1) and 20x film/a1vco data (response=9).
c this also alows other hypoellipse users to use the first position to
c specify calibrationc parameters and to leave the amplitude source codes blank.
            kluse(i) = klas(1, kdx(i))
cd          print *, 'amp. source was e, v, 1, 4, or blank'
cd          print *, 'kluse = ', kluse(i)
          else if (ampsc .eq. '2') then
c use a1vco/cusp fm tape calibration, system response 10
            kluse(i) = 10
          else if ( (ampsc .eq. 'p') .or. (ampsc .eq. 'o') .or.
     *              (ampsc .eq. 'g') .or. (ampsc .eq. 'k')) then
c use a1vco/pcelog calibration, system response 11
            kluse(i) = 11
          else if (ampsc .eq. 's') then
c use a1vco/semens playback, system response 12
            kluse(i) = 12
          else if ( (ampsc .eq. 'd') .or. (ampsc .eq. 'j') .or.
     *              (ampsc .eq. 'x') .or. (ampsc .eq. 'a')) then
c use a1vco/u of a masscomp, system response 13
            kluse(i) = 13
          else
c odd amplitude source, so do not compute xmag
            goto 30
          endif
c now find a c10 value that corresponds to this system response
          do 17 j = 1, 5
            if (kluse(i) .eq. klas(j, kdx(i))) then
              cluse(i) = calr(j, kdx(i))
cd            print *, 'use c10 = ', cluse(i)
              goto 18
            endif
17        continue
c no c10 for this k value, so skip xmag
cd        print *, 'no c10 for this k value, so skip xmag'
          goto 30
c
18        if (cluse(i) .lt. 0.01) goto 30
          k = kluse(i)
cd        print *, 'use response function # ', k
          if ((k.lt.0) .or. (k.gt.17)) goto 30
          xlmr = 0.
          if (k .eq. 0) goto 20
          if (k .le. 8) then
            if ((peruse(i).gt.3.162) .or. (peruse(i) .lt. 0.040))
     *      goto 30
            fq = 10.*alog10(1./peruse(i)) + 6.
            ifq = fq
            xlmr = rspa(k,ifq)
            if(ifq .lt. 20) then
              xlmr = xlmr + (fq-ifq)*(rspa(k,ifq+1) - rspa(k,ifq))
            endif
          else
            if ((peruse(i) .gt. 79.432) .or.
     *      (peruse(i) .lt. 0.010)) goto 30
            fq = 10.*alog10(1./peruse(i)) + 20.
            ifq = fq
            n = k - 8
            if (n .gt. iexcal) then
cd            print *,
cd   *        'can"t use a calibration curve that hasn"t been defined!'
cd            print *, 'n, k, iexcal = ', k, n, iexcal
              goto 30
            endif
            xlmr = qspa(n, ifq)
	    if(qspa(n, ifq) .eq. 0.) goto 30
            if(ifq .lt. 40) then
	      if(qspa(n, ifq+1) .eq. 0.) goto 30
              xlmr = xlmr + (fq-ifq)*(qspa(n,ifq+1) - qspa(n,ifq))
            endif
          endif
c compute magnification of station for period peruse(i)
c magnification (sysmag) is in measurement units per mm of ground displacement
c note that log(magnification in counts/mm) = xlmr + log(wood anderson magnification)
          alwamag = alog10( wa_magn(test(52), peruse(i)) )
c         print *, 'For station = ', phcard(keyphi(i))(1:4)
c         print *, 'amp = ', ampuse(i), 'counts'
c         print *, 'distance (rad2) = ', rad2
c 	  print *, 'WA magnification for period ', peruse(i), ' is ', 
c      *    wa_magn(test(52), peruse(i))
c         print *, ' xlmr = ', xlmr, ' cluse(', i,') = ', cluse(i)
          almgnif = xlmr + alwamag
          sysmag(i) = cluse(i)*(10.**almgnif)
c 	  print *, 'sysmag = ', sysmag(i)
c compute ptp gound motion in microns
	  gndmot(i) = (ampuse(i)*10**3)/sysmag(i)
c	  print *, 'ground motion in microns = ', gndmot(i)
c         
20        blac = alog10( ampuse(i)/(2.*cluse(i)) ) - xlmr
          rld2 = alog10(rad2)
          blnt = zmc1 - pwc1*rld2
          if (rad2 .ge. 40000.) blnt = zmc2 - pwc2*rld2
c xmag based on maximum amplitude
          xmag(i) = blac - blnt + xmgc(kdx(i))
c         print *, ' xmag = ', blac, ' - ', blnt, ' + ', xmgc(kdx(i))
c if xmgc is .ge. 6, then subtract 10 and do not
c include in average.
          if (xmgc(kdx(i)) .ge. 6.) then
            xmag(i) = xmag(i) - 10.
            goto 30
          endif
        endif
c       if ((rad2.lt.1.) .or. (rad2.gt.360000.)) then
        if ((rad2.lt.1.) .or. (rad2.gt.2250000.)) then
          xmag(i) = blank
	  goto 30
        endif
        if (xmwt(kdx(i)) .ne. 0) then
          nm = nm + 1
          avxm = avxm + xmag(i)
          sdxm = sdxm + xmag(i)**2
	  xtemp(nm) = xmag(i)
        endif

c fmag calculation
30      fmag(i) = blank
        kodsym(i) = ' '
        if (delta(i) .eq. 0.0) goto 40
        fms(i) = 0.
        if (fmp(i) .eq. blank) goto 40
        if (fmgc(kdx(i)) .eq. 0.) goto 40
        fms(i) = fmp(i) - t(i)*(test(1) - 1.)
        if (ksmp(i) .eq. 0) fms(i) = fmp(i) - t(i)
        if (ldx(i) .eq. 0) goto 31
        k = ldx(i)
        if ((abs(x(4,i))+abs(x(4,k))) .gt. fmp(i)/10.) goto 31
        if ((wt(i) .ne. 0.) .and. (wt(k) .ne. 0.)) fms(i) =
     *  fmp(i) - (tp(k) - tp(i))
31      if (fms(i) .lt. 0.1) fms(i) = 0.1
        if ((fmp(i)/fms(i)) .lt. 5.) goto 32
        kodsym(i) = 's'
        goto 40
32      fmpors = fmp(i)
        if (iuses .eq. 1.) fmpors = fms(i)
        ftouse = fmpors*fmgc(kdx(i))
        alogf = alog10(ftouse)
c fmag based on duration of signal
        fmag(i) = test(31) + test(32)*alogf + test(33)*delta(i) +
     *  test(40)*z + test(43)*alogf*alogf
        if (fmwt(kdx(i)) .ne. 0) then
          nf = nf + 1
c 5/27/92 do simple on or off rather than weighted mean
c         avfm = avfm + fmag(i)*fmwt(kdx(i))
c         sdfm = sdfm + (fmag(i)*fmwt(kdx(i)))**2
          avfm = avfm + fmag(i)
          sdfm = sdfm + fmag(i)**2
c         anf = anf + fmwt(kdx(i))
	  ftemp(nf) = fmag(i)
	endif
40    continue
c finish loop
c----
      if (nm .eq. 0) then
        avxm = blank
	xmmed = blank
      else
c avxm is average of nm xmags
        avxm = avxm/nm
        if (nm .ge. 2) then
          sdxm = sqrt(abs(sdxm/nm-avxm**2))
        else
          sdxm = 0.
        endif
c xmmed is the median xmag
	call median(xtemp, nm, xmmed, ierr)
      endif
      if (nf .eq. 0) then
        avfm = blank
	fmmed = blank
      else
c avfm is weighted average of nf fmags
c       avfm = avfm/anf
        avfm = avfm/nf
        if (nf .ge. 2) then
c         sdfm = sqrt(abs(sdfm/anf-avfm**2))
          sdfm = sqrt(abs(sdfm/nf-avfm**2))
        else
          sdfm = 0.0
        endif
c fmmet is the median fmag
	call median(ftemp, nf, fmmed, ierr)
      endif
      if ((nm .eq. 0) .and. (nf .eq. 0)) return
c----
c use all stations in preferred magnitude calculation

      if((imag .eq. 0) .or.
     *  ((imag .eq. 3) .and. (nf .eq. 0)) .or.
     *  ((imag .eq. 4) .and. (nm .ne. 0)) .or.
     *  ((imag .eq. 2) .and. (nf .eq. 0))) then
c xmag
        mag = avxm
        if(medmag) mag = xmmed

        if (nm .gt. 0) mgndx = 'x'
      else if((imag .eq. 1) .or.
     *       ((imag .eq. 4) .and. (nm .eq. 0)) .or.
     *       ((imag .eq. 3) .and. (nf .ne. 0)) .or.
     *       ((imag .eq. 2) .and. (nm .eq. 0))) then
c fmag
        mag = avfm
        if(medmag) mag = fmmed
        if (nf .gt. 0) mgndx = 'f'
      else if(imag .eq. 2) then
c average
        mag = 0.5*(avxm + avfm)
        if(medmag) mag = 0.5*(xmmed + fmmed)
        mgndx = 'a'
      endif
200   return
      end

	real function wa_magn(wa_static_mag, period)
c use Wood Andersion natural frequency = 1.25 Hz
	parameter (fzsq = 1.25*1.25)
c use Wood Anderson beta = 0.8
	parameter (betasq = 0.8*0.8)

	if(period .eq. 0.) then
	  wa_magn = -1.0
	else
	  fsq = 1./(period*period)
	  anumer = wa_static_mag*fsq
	  denom = sqrt((fzsq - fsq)**2 + 4.*betasq*fzsq*fsq)
	  wa_magn = anumer/denom
	endif

	return
	end
c end xfmags
c zstats.for    []
      subroutine zstats
c calculate the statistics of the residuals.
      include 'params.inc' 
      parameter (ndly = 11)
      integer punt
      common /punt/ punt
      common /bz/ na
      common /bqz/ avrps,avuse
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /orz/ onf
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /phoqn/ inst,knst
      common /zqr/ fno
      common /qmost/ wt(npa),z
      common /tmost/ x(4,npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      double precision sum(5),xwt
c nrwt is the total number of readings with weight .gt. zero.
c fno is the sum of p, s and s-p weights.
c onf is the sum of p and s weights.
      do 50 i = 1,5
        sum(i) = 0.0d0
50    continue
      nrwt = 0
      nswt = 0
      fno = 0.0
      onf = 0.0
      avr = 0.0
      aar = 0.0
      avrps = 0.0
      rms = 0.0
      if((iprn .ge. 5) .and. (na .eq. 0)) then
        write(punt, 60)
   60   format(//'          i    weight      ksmp             residual',
     *   '  zstats formats 80 and 1000')
      endif
      do 100 i = 1,nr
        if (wt(i) .eq. 0.0) goto 80
        nrwt = nrwt + 1
        fno = fno + wt(i)
        if (i .gt. nrp) nswt = nswt + 1
        onf = onf + wt(i)*ksmp(i)
        xwt = x(4,i)*wt(i)
        sum(1) = sum(1) + xwt
        sum(2) = sum(2) + abs(xwt)
        sum(5) = sum(5) + xwt*ksmp(i)
   80   if((iprn .lt. 5) .or. (na .ne. 0)) goto 100
        write(punt,1000) i,wt(i),ksmp(i),x(4,i)
 1000   format(i10,f12.4,i10,f20.2)
  100 continue
      if(nrwt .eq. 0) goto 500
      avr = sum(1)/fno
      aar = sum(2)/fno
      if(onf .eq. 0.0) goto 125
      avrps = sum(5)/onf
      fixor = 1.0
      if(inst .eq. 8) fixor = 0.0
  125 do 150 i = 1,nr
  150 sum(3) = sum(3) + wt(i)*((x(4,i)-avrps*fixor*ksmp(i))**2)
      rms = sqrt(sum(3)/fno)
      if((iprn .lt. 4) .or. (na .ne. 0)) goto 500
      write(punt,1010) avr,aar,avrps,rms,nrwt,fno,onf
 1010 format(/, '       avr       aar     avrps         rms      nrwt',
     * '       fno        onf   zstats format 1010', /,
     * 1x, 3f10.3, e15.8, i10, 2f10.2)
  500 avuse = avrps
      if(inst .eq. 8) avuse = 0.0
      return
      end
c end zstats
c npunch.for    [unix]
      subroutine npunch(style)
c----
c print summary record with regress information
c write summary record with regress information
c write summary record without regress information
c write station data
      include 'params.inc' 
      parameter (ndly = 11)
      integer sumout, arcout
      character*5 style, upstrg*1
c style can be 'final' or 'temp '.  if 'temp ', then write output
c on 4 to temporaty file 14 and output on 11 to temporary file 15
      logical sumpr
      real bat2,bon2,mag
      character*1 iqpr, iwrk, fmit*6, dnstrg
      character aline*133, pline*117
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /an/ n14, n15
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dbiln/ ioldq
      common /dhin/ iglob, zup, zdn
      logical medmag
      common /dinx/ imag,imagsv,medmag
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      common /omnfh/ dmin,dmin3,sminp
      common /obcfn/ ain(npa)
      common /ocfn/ adj,seh,lat1,bat2,lon1,bon2,igap
      character*1 isnla, isnlo, krm1, krm2
      common /ocfn1/ isnla, isnlo, krm1, krm2
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /pox/ fmp(npa),prx(npa),icent1,icent2
      integer punt
      common /punt/ punt
      common /ghnq/ iexit
      common /hpn/ wslope
      common /idno/ ksel,ksort
      common /ilotv/ elvdly(npa)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /in/ irmo,iryr,odmag
      character*1  magke
      common /in1/ magke
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
c if instset .ne. ' ', then put this new value of inst on summary record
      character*1 instset
      common /hnu/ instset
      common /ofnv/ kmin
      character*1 nq
      common /ofnv1/ nq
      common /ofln/ sec
      common /olnx/ xmag(npa),fmag(npa),avxm,avfm,xmmed,fmmed
      common /on/ ix,iy,iz,ax1,axz
      character iqax*1
      common /on1/ iqax
      common /onx/ kluse(npa), cluse(npa), peruse(npa), ampuse(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pgnoqv/ p(npa),jmin(npa)
      common /pgnov/ dt(npa)
      common /pgoq/ s(npa)
      character*4 ipro, ichec, evtype*1, evstat*1, root*256
      common /dbhip/ ipro, ichec, evtype, evstat, root
      common /phoqn/ inst,knst
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pn/ itpused(npa)
      common /pnl/ ww(npa)
      common /pno/ lph,keyph(npa),nsum
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pnoqtx/ ldx(npa)
      common /qmost/ wt(npa),z
      common /qgnotx/ delta(npa)
      common /qgo/ org
      common /rbno/ idip(3),iaaz(3),a(3,3)
      common /rfgnoq/ se(4)
      common /ro/ yse,seorg,phi
      common /tmost/ x(4,npa)
      common /tonxb/ t(npa),fms(npa)
      common /xfmno/ mag
      character*1 mgndx
      common /xfmno1/ mgndx
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      character*(4) tmpsum_type, tmparc_type, fname*256
      character acent2*2, fooline*133
      integer tmpsum_unit, tmparc_unit
      parameter (tmpsum_type  = '.3sc')
      parameter (tmparc_type  = '.4sc')
      parameter (tmpsum_unit  = 14)
      parameter (tmparc_unit  = 15)

      write(acent2, '(i2.2)') icent2
c* (unix
      close(tmpsum_unit)
      length_root = lentru(root)
      fname = root(1:length_root)//tmpsum_type
      call openfl(tmpsum_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 300)

      close(tmparc_unit)
      fname = root(1:length_root)//tmparc_type
      call openfl(tmparc_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 0)
c* unix)
c* (pc
c      rewind tmpsum_unit 
c      rewind tmparc_unit 
c* pc)
c* (vax
c      rewind tmpsum_unit 
c      rewind tmparc_unit 
c* vax)

      sumpr = .false.
      rmag = mag
c n14 and n15 count the number of records saved temporarily, until
c geterr has been run to compute zup and zdn
      n14 = 0
      n15 = 0
c set up unit numbers for output
      if(style .eq. 'final') then
        sumout = 4
        arcout = 11
      else
        sumout = tmpsum_unit
        arcout = tmparc_unit
      endif
c	write(punt, '(a, 2i5)') 'sumout, arcout = ', sumout, arcout
      if((magke .eq. ' ') .or. (magke .eq. 'x') .or.
     *   (magke .eq. 'f') .or. (magke .eq. 'a') .or.
     *   (magke .eq. 'k')) goto 3
c     in this case, save old magnitude value
      rmag = odmag
      mgndx = magke
    3 nfile = 4
      if(ipun .lt. 0 .or. ipun .gt. 4) return
c fix up the date and time 
      khr = ihr
      kmin = lbastm
      sec = org
      kkdate = kdate
      call tshift(kkdate,khr,kmin,sec,dum)

c take care of the fake solution cases
      if(iexit .eq. 1) then
c       don't write out fake arrival data if there wasn't a solution
        if (ipun .eq. 4) return
	if ((inst .eq. 9) .and. (keyph(1) .eq. -2)) then
c         for inst=9, use original summary record if there was one
          pline = acent2//phcard(1)(1:115)
	  goto 137
        else if((dnstrg(evtype) .eq. 't') .or.
     *    (dnstrg(evtype) .eq. 'n') .or.
     *    (dnstrg(evtype) .eq. 'r')) then
c         for tele, nuclear, or regional, use old summary record
          pline = acent2//phcard(1)(1:115)
	  goto 137
	else
          goto 225
	endif
      endif
c
c key to keyph values
c      -1  comment record
c      -2  summary record
c      -3  instruction record
c      -4  deleted station record
c     -11  decode error
c      .ge. 1, index number of phase used in solution
c
c print and write summary information with regress information
   10 call formf(sec,iisec,4,2)
      call formf(bat2,iilat,4,2)
      call formf(bon2,iilon,4,2)
      call formf(z-test(8),iiz,5,2)
      call formf(dmin,iidmin,3,0)
      call formf(rms,iirms,4,2)
      iqpr = iqax
      if(ioldq .eq. 1) iqpr = nq
      if(iprn .lt. 3) goto 130
      write(punt, '(3a)')
     *  '     date   origin      lat      long    depth    mag no',
     *  ' gap  d3   rms az/dp    se az/dp    se az/dp    se  iqpr',
     *  ' isetno nswt  seq  s-p'
c
c*********************create summary record for print file**************
      aline = ' '

c     write(punt, '(a,2i10, f10.2, 2i10 )') 
c    *  'khr, kmin, sec, kdate, kkdate',
c    *  khr, kmin, sec, kdate, kkdate
      write(aline, 126)  kkdate, khr, kmin, sec, lat1, isnla, bat2,
     * lon1, isnlo, bon2, z-test(8)
  126 format(1x, i6.6, 1x ,i2, i2, f6.2, i3, a1, f5.2, i4, a1,
     * f5.2, 1x, f6.2)
c
      if (rmag .ne. blank) then
        call formit(rmag, frmag, fmit, 3, 1)
        write(aline(49:51), fmit) frmag
      endif
c
      write(aline(52:74), 127) nrwt, igap, dmin, rms, iaaz(ix), idip(ix)
  127 format(i3, i4, f5.0, f5.2, i3, '/' ,i2)
c
      if(se(ix) .ne. blank) then
        call formit(se(ix), frsei, fmit, 5, 1)
        write(aline(76:80), fmit) frsei
      endif
c
      write(aline(81:86), 128) iaaz(iy), idip(iy)
  128 format(i3, '/' ,i2)
c
      if (se(iy) .ne. blank) then
        call formit(se(iy), frsej, fmit, 5, 1)
        write(aline(88:92), fmit) frsej
      endif
c
      write(aline(93:98), 128) iaaz(iz), idip(iz)
c
      if (se(iz) .ne. blank) then
        call formit(se(iz), frsek, fmit, 5, 1)
        write(aline(100:104), fmit) frsek
      endif
c
c     write(aline(105:132), 129) iqpr, mgndx, nswt, seq, sminp
c 129 format(5x ,a1, 6x ,a1, 3x ,i2, a5, f5.2)
      write(aline(105:127), 129) iqpr, mgndx, nswt, seq
  129 format(5x ,a1, 6x ,a1, 3x ,i2, a5)
      if(sminp .ne. blank) then
        write(aline(128:132), '(f5.2)') sminp
      endif
c
c     add century
      fooline = aline(2:132)
      aline = acent2//fooline
      write(punt, '(1x, a)') aline
c********************finished with printed summary record****************
c
  130 continue
c*********************create summary record******************************
      pline = ' '
      write(pline, 131) kkdate, khr, kmin, iisec, lat1, isnla, iilat,
     * lon1, isnlo, iilon
  131 format(            i6.6,  i2,   i2,    i4,   i2,    a1,    i4,
     *   i3,    a1,    i4)
      write(pline(111:115), '(i5)') iiz
      if(test(9) .eq. 0.0) then
        write(pline(30:34), '(i5)') iiz
      else
        if(iiz .lt. 0) then
          write(pline(30:34), '(a)') '  -00'
        else
          write(pline(30:34), '(i5)') iiz
        endif
      endif
c
      if (rmag .ne. blank) then
        call riorbk(rmag, jmag, fmit, 2, 1)
        write(pline(35:36), fmit) jmag
      endif
c
      write(pline(37:54), 133) nrwt, igap, iidmin, iirms, iaaz(ix),
     * idip(ix)
  133 format(                    i3,   i3,     i3,    i4,       i3,
     *       i2)
c
      if (se(ix) .ne. blank) then
        call riorbk(se(ix), iisei, fmit, 4, 2)
        write(pline(55:58), fmit) iisei
      endif
c
      write(pline(59:63), '(i3, i2)') iaaz(iy), idip(iy)
c
      if (se(iy) .ne. blank) then
        call riorbk(se(iy), iisej, fmit, 4, 2)
        write(pline(64:67), fmit) iisej
      endif
c
      if(medmag) then
c use median xmag
        if (xmmed .ne. blank) then
          call riorbk(xmmed, iavxm, fmit, 2, 1)
          write(pline(68:69), fmit) iavxm
        endif
      else
c use average xmag
        if (avxm .ne. blank) then
          call riorbk(avxm, iavxm, fmit, 2, 1)
          write(pline(68:69), fmit) iavxm
        endif
      endif
c
      if(medmag) then
c use median fmag
        if (fmmed .ne. blank) then
          call riorbk(fmmed, iavfm, fmit, 2, 1)
          write(pline(70:71), fmit) iavfm
        endif
      else
c use average fmag
        if (avfm .ne. blank) then
          call riorbk(avfm, iavfm, fmit, 2, 1)
          write(pline(70:71), fmit) iavfm
        endif
      endif
c
      write(pline(72:72), '(a1)') evstat
c
      if (se(iz) .ne. blank) then
        call riorbk(se(iz), iisek, fmit, 4, 2)
        write(pline(73:76), fmit) iisek
      endif
c
      write(pline(77:96), 134) iqpr, mgndx, nswt,    ipro,
     * irmo, iryr, evtype, phcard(lph)(19:19), seq
  134 format(                    a1,    a1,   i2, '/', a4,
     *   i2,   i2,     a1,                 a1,  a5)
      if(instset .ne. ' ') pline(91:91) = instset
c
      if(sminp .ne. blank) then
        call riorbk(sminp, isminp, fmit, 4, 2)
        write(pline(97:100), fmit) isminp
      endif
c
c     if(iglob .eq. 0) then
      if((iglob .eq. 0) .and. ((inst .eq. 0) .or. 
     *  (inst .eq. 8))) then
        call formal(zup, izup, 2, 0, fmit, azup)
        if (fmit .eq. ' ') then
          write(pline(101:102), '(i2)') izup
        else
          write(pline(101:102), fmit) azup
        endif
        call formal(zdn, izdn, 2, 0, fmit, azdn)
        if (fmit .eq. ' ') then
          write(pline(103:104), '(i2)') izdn
        else
          write(pline(103:104), fmit) azdn
        endif
      endif
c
      if (wslope .ne. 0.) then
        call riorbk(wslope, iiwslp, fmit, 4, 2)
        write(pline(105:108), fmit) iiwslp
      endif
c
c compute the number of phases weighted out due to large residuals
      nwtout = 0
      do 136 i = 1, lph
        if(keyph(i) .gt. 0) then
          k = keyph(i)
c	  print *, 'nwtout, k, kwr(k) ', nwtout, k, kwr(k)
c p phases
          if((kwr(k) .eq. 'b') .or. (kwr(k) .eq. 'm') .or.
     *      (kwr(k) .eq. 'j')) nwtout = nwtout + 1
          if(ldx(k) .ne. 0) then
c s phases
            kk = ldx(k)
            if((kwr(kk) .eq. 'b') .or. (kwr(kk) .eq. 'm') .or.
     *        (kwr(kk) .eq. 'j')) nwtout = nwtout + 1
          endif
        endif
136   continue
      if(nwtout .gt. 99) nwtout = 99
      write(pline(109:110), '(i2)') nwtout
c
c     for now (12/21/91) keep upper case characters
      pline(17:17) = upstrg(pline(17:17))
      pline(25:25) = upstrg(pline(25:25))
      pline(77:77) = upstrg(pline(77:77))
      pline(78:78) = upstrg(pline(78:78))
c
c     add century
      fooline = pline(1:115)
      pline = acent2//fooline
c
c*************write out current summary record parameters********
c	write(punt, '(a, i5)') 'ipun = ', ipun
137   if (ipun .lt. 3) then	
        write(sumout, '(a)') pline
c keep the summary file up to date
c* (unix
	if(sumout .eq. 4) call flush(sumout)
c* unix)
c* (pc 
c* pc) 
c* (vax 
c* vax) 
c	write(punt, '(a, i5)') 'just flushed sumout unit = ', sumout
        if(style .ne. 'final') n14 = n14 + 1
      endif
      if (ipun .eq. 1) return
      goto 260
c
c****************come here if no solution was found**************
  225 continue
c before writing out a fake summary record, check if the current
c summary record (if there is one) is also a fake, based on having
c a blank rms field (cols 46:49)
      if(((phcard(1)(46:49) .ne. '    ') .and.
     *   (keyph(1) .eq. -2)) .or.
     *   (keyph(1) .ne. -2)) then
c there is a valid summary record from a previous run to preserve, or
c there is no summary record from previous run, so
c generate a fake summary record
        ibegin = 1
        pline = ' '
	if(keyph(1) .eq. -2) then
	  pline(1:10) = phcard(1)(1:10)
	else
          write(pline, '(i6.6, i4)') kkdate, khrmn
	endif
c
        pline(17:17) = 'n'
        pline(25:25) = 'w'
        if(rmag .ne. blank) then
          call riorbk(rmag, jmag, fmit, 2, 1)
          write(pline(35:36), fmit) jmag
        endif
c
        if(avxm .ne. blank) then
          call riorbk(avxm, iavxm, fmit, 2, 1)
          write(pline(68:69), fmit) iavxm
        endif
c
        if (avfm .ne. blank) then
          call riorbk(avfm, iavfm, fmit, 2, 1)
          write(pline(70:71), fmit) iavfm
        endif
c
        write(pline(72:72), '(a1)') evstat
c
        pline(78:81) = 'k  /'
c
        write(pline(82:96), 227) ipro, irmo, iryr, evtype,
     *  phcard(lph)(19:19), seq
  227   format(a4, 2i2, 2a1, a5)
        if(instset .ne. ' ') pline(91:91) = instset

c       for now (12/21/91) keep upper case characters
        pline(17:17) = upstrg(pline(17:17))
        pline(25:25) = upstrg(pline(25:25))
        pline(77:77) = upstrg(pline(77:77))
        pline(78:78) = upstrg(pline(78:78))
	fooline = acent2//pline(1:115)
	pline = fooline
      else
c there was already a fake summary record, so 
c do not generate another fake summary record
        ibegin = 2
        if(phcard(1)(81:81) .eq. '/') then
	  pline = acent2//phcard(1)
	else
	  pline = phcard(1)
	endif
      endif
c
      if(ipun .lt. 3) then
        write(sumout, '(a)') pline
c keep the summary file up to date
c* (unix
	if(sumout .eq. 4) call flush(sumout)
c* unix)
c* (pc 
c* pc) 
c* (vax 
c* vax) 
        if(style .ne. 'final') n14 = n14 + 1
      endif
c
      if(ipun .eq. 1) return
c for archive option, write out first old summary record if it was
c real, followed by all privious summary and phase records
      write(arcout, '(a)') pline(1:lentru(pline))
      if(style .ne. 'final') n15 = n15 + 1
      do 228 i = ibegin, lph
	if(keyph(i) .eq. -2) then
c* (unix
          phcard(i)(81:81) = '\\'
c* unix)
c* (vax
c* (pc
c         phcard(i)(81:81) = '\'
c* pc)
c* vax)
          write(arcout, 144) acent2//phcard(i)(1:lentru(phcard(i)))
	else
          if(upstrg(phcard(i)(1:2)) .ne. 'C*')
     *      phcard(i)(65:65) = upstrg(phcard(i)(65:65))
          write(arcout, 144) phcard(i)(1:lentru(phcard(i)))
          if(style .ne. 'final') n15 = n15 + 1
	endif
  228 continue
      return
c
c **************output archive-phase data for each station*************
  260 iqpr = iqax
      if(ioldq .eq. 1) iqpr = nq
c archive current summary record parameters
      write(arcout, '(a)') pline(1:lentru(pline))
      if(style .ne. 'final') n15 = n15 + 1
      nott = 0
c
c********************** loop through stations
      do 310 i = 1, lph
      pline = ' '
c skip on down if ipun = 4 and generate perfect data
      if(ipun .eq. 4) then
        if(keyph(i) .lt. 1) goto 310
        goto 302
      endif
c old summary records, skip the first one, be sure col 81 has \
c     for the secondary summary records
      if(keyph(i) .eq. -2) then
        nott = nott + 1
        if(nott .eq. 1) goto 310
c* (unix
        phcard(i)(81:81) = '\\'
c* unix)
c* (vax
c* (pc
c        phcard(i)(81:81) = '\'
c* pc)
c* vax)
        pline = acent2//phcard(i)(1:115)  
        write(arcout, 144) pline(1:lentru(pline))
  144   format(a)
        if(style .ne. 'final') n15 = n15 + 1
        goto 310
      endif
      if(keyph(i) .le. 0) then
c   write out comment record, deleted station record or instruction record
        write(arcout, 144) phcard(i)(1:lentru(phcard(i)))
        if(style .ne. 'final') n15 = n15 + 1
        goto 310
      endif
      k = keyph(i)
      kji = kdx(k)
      call formf(x(4,k),ipr,5,2)
      iain = ain(k)  + 0.5001
      iwrk = '    '
      isr = 0
      isse = 0
      if(ldx(k) .ne. 0) then
c s data
        kk = ldx(k)
        iwrk = kwr(kk)
        call formf(x(4,kk),isr,5,2)
        if(wt(kk) .ne. 0.0) then
          setmp = yse/sqrt(wt(kk))
          call formal(setmp, isse, 3, 2, fmit, xsse)
          if(fmit .eq. '      ') then
            write(pline(81:83), '(i3)') isse
          else
            write(pline(81:83), fmit) xsse
          endif
        else
          write(pline(81:83), '(a)') '99.'
        endif
        goto 300
      endif
      if(ksmp(k) .ne. 1) then
c s-p data
        call formf(x(4,k),isr,5,2)
      endif
  300 continue
      call formf(dly(kno,kji),ipdly,3,1)
      call formf(sdly(kno,kji),isdly,3,1)
      call formf(elvdly(k),ieldly,3,1)
      call formf(delta(k),idelt,4,1)
      call formf(az(k),iaz,3,0)
c
      write(pline(1:50), 301) phcard(i)(1:24), idelt, iaz,
     * phcard(i)(32:40), iain, phcard(i)(44:50)
  301 format(a24, i4, i3, a9, i3, a7)
c
      call formal(t(k), iptt, 4, 2, fmit, xiptt)
      if(fmit .eq. '      ') then
        write(pline(51:54), '(i4)') iptt
      else
        write(pline(51:54), fmit) xiptt
      endif
      if(wt(k) .ne. 0.0) then
        setmp = yse/sqrt(wt(k))
        call formal(setmp, ise, 3, 2, fmit, xse)
        if(fmit .eq. '      ') then
          write(pline(55:57), '(i3)') ise
        else
          write(pline(55:57), fmit) xse
        endif
      else
        write(pline(55:57), '(a)') '99.'
      endif
c
      write(pline(58:58), '(a1)') kwr(k)
c save the instrument period and gain characters
      write(pline(59:60), '(a)') phcard(i)(59:60)
c
c replace calr with siemens gain state and a1vco gain range state
      write(pline(61:62), '(a)') phcard(i)(61:62)
c
      write(pline(63:80), 303) phcard(i)(63:75), ipr
  303 format(a13, i5)
c
      write(pline(84:98), 3031) iwrk, isr, ipdly,
     * isdly, ieldly
3031  format(a1, i5, 3i3)
c
      if (xmag(k) .ne. blank) then
        write(pline(99:100), '(i2)') kluse(k)
        call riorbk(xmag(k), ixmag, fmit, 2, 1)
        write(pline(101:102), fmit) ixmag
      endif
c
      if (fmag(k) .ne. blank) then
        call riorbk(fmag(k), ifmag, fmit, 2, 1)
        write(pline(103:104), fmit) ifmag
      endif
c
c satellite hops
c value of nhop is no longer preserved, because the station history can
c accomodate two different telemetry delays
c      if(phcard(i)(110:110) .ne. ' ') then
c preserve any non blank value
c        pline(105:110) = phcard(i)(105:110)
c      else
c note that itpused is based soley on the p-phase
      if (itpused(k) .ne. 0) then
        nhop = -tpdly(itpused(k), kji)/.27 + .1
      else
        nhop = 0
      endif
      write(pline(105:110), '(a, i1)') phcard(i)(105:109), nhop

c     for now (12/21/91) keep upper case 
c     if(pline(1:2) .ne. 'C*') then
      if(upstrg(pline(1:2)) .ne. 'C*') then
        pline(58:58) = upstrg(pline(58:58))
        pline(65:65) = upstrg(pline(65:65))
        pline(84:84) = upstrg(pline(84:84))
      endif
c
      write(arcout, '(a)') pline(1:lentru(pline))
      if(style .ne. 'final') n15 = n15 + 1
      goto 310
c
c***********************genterate perfect data*********************
  302 k = keyph(i)
      read(phcard(i), 3021) ldate, lhr, lmin
3021  format(9x, i6.6, 2i2)
      psec = p(k) - x(4,k)
      if(ldx(k) .ne. 0) ssec = s(k) - x(4,ldx(k))
      call tshift(ldate,lhr,lmin,psec,ssec)
      call formf(psec, ipsec, 5, 2)
      if(ldx(k) .ne. 0) then
c       take care of s phase
        call formf(ssec, issec, 5, 2)
        write(arcout, 304) phcard(i)(1:9), ldate, lhr, lmin, ipsec,
     *  issec, phcard(i)(37:40)
        if(style .ne. 'final') n15 = n15 + 1
  304   format(a9, i6.6, 2i2, i5, 7x, i5, a4)
      else
c       only have p to write out
        write(arcout, 305) phcard(i)(1:9), ldate, lhr, lmin, ipsec
  305   format(a9, i6.6, 2i2, i5)
        if(style .ne. 'final') n15 = n15 + 1
      endif
  310 continue
      if(ipun .eq. 4) then
        write(arcout,320)
  320   format('                      ')
        if(style .ne. 'final') n15 = n15 + 1
      endif
      return
      end
c end npunch
c opfls.for    [unix]
c ******************************************************************
c ***          subroutine opfls of program hypoellipse          ***
c ******************************************************************
c ***   a rewritten version of the opfls subroutine from usgs.   ***
c ***  this version works satisfactorily when run interactively  ***
c ***    or with its input "redirected" from a file or shell     ***
c ***               script.  ghcs at uaf, 5/17/88.               ***
c ******************************************************************
      subroutine opfls (iplt, inmain, root, nttab)
c* (pc
c$notruncate
c* pc)
c
c     for the masscomp compiler, open statements must have form='noprint'
c     for all files to be written, except the print and log files.
c --- the iplt and inmain arguments will be set to return the
c --- filecode from which the main input file should be read.
c --- that is, either from a separate file (unit 8), or the
c --- standard input stream (unit 5) which may be a redirected
c --- c-shell "here document" containing shell variables.
c --- the ilis argument will be set to return the initial, or
c --- default value of ilis, a new option variable controlling
c --- the verboseness of the "printer" output file (unit 9).
c --- it provides a mechanism for generating an output file
c --- which contains only the information vital to evaluating
c --- the "correctness" of a phaselist and location.
c --- 7/29/89 note, jcl:
c      ilis is now set by the constants print option (see 2.2.3.20 in
c      the hypoellipse manual.
      integer iplt, inmain
c --- lengths of filename and type strings and the input
c --- and output unit numbers for this routine.
      integer max_length, max_type, in, out
      parameter (max_length = 255)
      parameter (max_type = 4)
      parameter (in = 5)
      parameter (out = 6)
c --- logical unit numbers for the files to be opened.
      integer stdin_unit, main_unit, stderr_unit, punt
      integer print_unit, summary_unit, arch_unit, tb11_unit
      integer tb12_unit, tb13_unit, stacorr_unit, log_unit
      integer tmp1_unit, tmp2_unit, tmpsum_unit, tmparc_unit
      integer tmpmess_unit
c* (vax
c* (pc
c      parameter (stderr_unit  = 6)
c* pc)
c* vax)
c* (unix
      parameter (stderr_unit  = 0)
c* unix)
      parameter (stdin_unit   = 5)
      parameter (main_unit    = 8)
      parameter (print_unit   = 9)
      parameter (summary_unit = 4)
      parameter (arch_unit    = 11)
      parameter (tb11_unit    = 21)
      parameter (tb12_unit    = 22)
      parameter (tb13_unit    = 23)
      parameter (stacorr_unit = 13)
      parameter (log_unit     = 6)
      parameter (tmp1_unit    = 2)
      parameter (tmp2_unit    = 3)
      parameter (tmpsum_unit  = 14)
      parameter (tmparc_unit  = 15)
      parameter (tmpmess_unit = 16)
c --- default input file and root/base strings.
      character*(max_length) in_def, root_def
      parameter (in_def = 'stdin')
      parameter (root_def = 'hypoe')
c --- the default file types (extensions) for output files.
      character*(max_type) print_type, summary_type, arch_type
      character*(max_type) stacorr_type,log_type, tmp1_type
      character*(max_type) tmp2_type, tmpsum_type, tmparc_type
      character*(max_type) tmpmess_type
      parameter (print_type   = '.out')
      parameter (summary_type = '.sum')
      parameter (arch_type    = '.arc')
      parameter (stacorr_type = '.nst')
      parameter (log_type     = '.log')
      parameter (tmp1_type    = '.1st')
      parameter (tmp2_type    = '.2st')
      parameter (tmpsum_type  = '.3sc')
      parameter (tmparc_type  = '.4sc')
      parameter (tmpmess_type = '.5sc')
c --- the lentru function returns the number of
c --- non-blank characters in its string argument.
c --- it returns zero if the string is completely blank.
      integer lentru
c --- local variable declarations.
      character*(*) root
      character*(max_length) fname, default, log_file
      character*(max_length) dnstrg
      character answer*1, openstat*7
      integer status, length
      integer length_def, length_root, length_log
      common /punt/ punt
      default = in_def
      length_def = lentru(default)
      write(out, '(a)') ' begin hypoellipse'
      write (out,
     *  '(/'' hypoellipse input filename ('',a,'')? '',$)')
     *  default(1:length_def)
      read (in, '(a)', iostat=status) fname
      length = lentru(fname)
      if ((status.ne.0) .or. (length.eq.0)) then
        fname = default
        length = length_def
      endif
      write (out, '(1x,a)') fname(1:length)
      if (dnstrg(fname(1:length)).eq.'stdin') then
        inmain = stdin_unit
        iplt = stdin_unit
      else
        inmain = main_unit
        iplt = main_unit
c       call openfl(iunit, ifile, istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl( iplt, fname, 'old', 'zero', 'readonly',
     *          'none', 0)
c       open (unit=iplt, file=fname(1:length), status='old',
c    *  blank='zero')
c ---   previous version used blank='zero' which is very troublesome
c ---   (e.g. integers must be right justified in their fields
c ---   which is not obvious when looking at an input file).
c ---   however, some summary records have imbedded blanks with the
c ---   yrmodyhrmn field, and will not be read correctly unless
c ---   blank = 'zero' is specified!
c ---   also, iostat=status gives less information than the operating
c ---   system when the open fails.
c       if (status.ne.0) then
c         write (out,
c    *      '('' hypoellipse failed to open input file "'',a,''"'')')
c    *      fname(1:length)
c         stop 'abort from module opfls.'
c       endif
      endif
c --- should the output files be opened with a status of
c --- "new" to prohibit overwriting existing ones of the
c --- same name, or "unknown" to allow overwriting?
      openstat = 'new'
c* (unix
      write (out,
     * '('' allow overwriting of existing output files (no)? '',$)')
      read (in, '(a)', iostat=status) answer
      answer = dnstrg( answer )
      if ((status.ne.0) .or. (answer.eq.' ')) answer = 'n'
      if (answer.eq.'y') then
        answer = 'y'
        openstat = 'unknown'
      else
        answer = 'n'
        openstat = 'new'
      endif
      write (out, '(1x,a)') answer
c* unix)
c --- output filename's root or basename used in the default
c --- names for the remaining files.
      default = root_def
      length_def = lentru(default)
      write (out, '('' root for output filenames ('',a,'')? '',$)')
     *  default(1:length_def)
      read (in, '(a)', iostat=status) root
      length_root = lentru(root)
      if ((status.ne.0) .or. (length_root.eq.0)) then
        root = root_def
      endif
      length_root = lentru(root)
c --- a kludge to fix the file names not passed as arguments.
c     if (root(length_root:length_root) .eq. 'p') then
c       root = root(1:length_root-1)
c       length_root = length_root - 1
c     endif
      write (out, '(1x,a)') root(1:length_root)
c --- summary of warning messages and final summary/log filename
      default = root(1:length_root)//log_type
      length_def = lentru(default)
c* (vax
c* (pc
c      write (out, '('' log filename or "screen" ('',a,'')? '',$)')
c* pc)
c* vax)
c* (unix
      write (out, '('' log filename or "stdout" ('',a,'')? '',$)')
c* unix)
     *  default(1:length_def)
      read (in, '(a)', iostat=status) log_file
      length_log = lentru(log_file)
      if ((status.ne.0) .or. (length_log.eq.0)) then
        log_file = default
        length_log = length_def
      endif
      write (out, '(1x,a)') log_file(1:length_log)
c --- we can't actually open the log file now, since we're still
c --- in the process of using unit 6 interactively...
c
c --- printer output file --------------------------------------
      punt = print_unit
      default = root(1:length_root)//print_type
      length_def = lentru(default)
c* (unix
      write (out,
     * '('' filename for printer output or "screen" ('',
     * a,'')? '',$)')
     *  default(1:length_def)
      read (in, '(a)', iostat=status) fname
c* unix)
c* (vax
c* (pc
c      write (out, '(a)') ' filename for printer output or "log"'
c      write (out,
c     * '('' answer "log" to combine with log file ('',
c     * a,'')? '',$)')
c     *  default(1:length_def)
c      read (in, '(a)', iostat=status) fname
c      fname = dnstrg( fname )
c      if (fname .eq. 'log') fname = 'screen'
c* pc)
c* vax)
      length = lentru(fname)
      if ((status.ne.0) .or. (length.eq.0)) then
        fname = default
        length = length_def
      endif
      write (out, '(1x,a)') fname(1:length)
      if (dnstrg(fname(1:length)) .eq. 'screen') then
        punt = stderr_unit
      else
c       call openfl(     iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl(punt, fname, openstat, 'null', 'none',
     *         'none', 0)
      endif
c     open (print_unit, file=fname(1:length), status=openstat)
c
c --- summary record filename --------------------------------------
      write (out,
     * '('' will this job generate a summary file (no)? '',$)')
      read (in, '(a)', iostat=status) answer
      if ((status.ne.0) .or. (answer.eq.' ')) answer = 'n'
      write (out, '(1x,a)') answer
      answer = dnstrg( answer )
      if (answer .eq. 'y') then
        default = root(1:length_root)//summary_type
        length_def = lentru(default)
        write (out,
     *    '('' summary output filename ('',a,'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
c       call openfl(       iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl(summary_unit, fname, openstat, 'null', 'none',
     *      'noprint', 0)
c       open (summary_unit,file=fname(1:length),status=openstat)
c    *        form = 'noprint')
      endif
c
c --- archive phase filename --------------------------------------
      write (out,
     *  '('' will this job generate an archive file (no)? '',$)')
      read (in, '(a)', iostat=status) answer
      if ((status.ne.0) .or. (answer.eq.' ')) answer = 'n'
      write (out, '(1x,a)') answer
      answer = dnstrg( answer )
      if (answer .eq. 'y') then
        default = root(1:length_root)//arch_type
        length_def = lentru(default)
        write (out, '('' archive output filename ('',a'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
c       call openfl(    iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl(arch_unit, fname, openstat, 'null', 'none',
     *      'noprint', 0)
c       open (arch_unit, file=fname, status=openstat)
c     *        form = 'noprint')
      endif
c
c --- travel time table filenames --------------------------------------
      write (out, '('' use travel time tables (no)? '',$)')
      read (in, '(a)', iostat=status) answer
      if ((status.ne.0) .or. (answer.eq.' ')) answer = 'n'
      write (out, '(1x,a)') answer
      answer = dnstrg( answer )
      if (answer .eq. 'y') then
        default = 'none'
        length_def = lentru(default)
        write (out, '('' filename for first table ('',a,'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
c       call openfl(    iunit, ifile, istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl(tb11_unit, fname, 'old', 'null', 'readonly',
     *         'none', 0)
c       open (tb11_unit, file=fname(1:length), status='old')
	nttab = 1
        default = 'none'
        length_def = lentru(default)
        write (out, '('' filename for second table ('',a,'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
        if (dnstrg(fname(1:4)) .ne. 'none') then
	  nttab = 2
          call openfl(tb12_unit, fname, 'old', 'null', 'readonly',
     *      'none', 0)
c         call openfl(    iunit, ifile, istat,  izero,   ishr,
c    *      iform, irecl)
	endif
        default = 'none'
        length_def = lentru(default)
        write (out, '('' filename for third table ('',a,'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
        if (dnstrg(fname(1:4)) .ne. 'none') then
          call openfl(tb13_unit, fname, 'old', 'null', 'readonly',
     *      'none', 0)
c         call openfl(    iunit, ifile, istat,  izero,   ishr,
c    *      iform, irecl)
	  if(nttab .ne. 2) then
	    write (out, '(a)') 'You may not define tables 11 and 13'
	    write (out, '(a)') 'without defining travel time table 12!'
	    stop
	  endif
	  nttab = 3
	endif
      endif
c
c --- optional relocation of events with revised delays --------------
      write (out, '('' does this job relocate events with revised '',
     *  ''station delays (no)? '',$)')
      read (in, '(a)', iostat=status) answer
      if ((status.ne.0) .or. (answer.eq.' ')) answer = 'n'
      write (out, '(1x,a)') answer
      answer = dnstrg( answer )
      if (answer .eq. 'y') then
        default = root(1:length_root)//stacorr_type
        length_def = lentru(root)
        write (out,
     *    '('' station delay output filename ('',a,'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
c       open (stacorr_unit, file=fname(1:length), status=openstat)
c    *        form = 'noprint')
c       call openfl(       iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl(stacorr_unit, fname, openstat, 'null', 'none',
     *      'noprint', 0)
      endif
c --- write out a blank record --------------------------------------
      write (out, *)
c --- we're done with interactive i/o using stdout (unit 6) so
c --- we can finally open the "log" file, if there is to be one.
c     fname = dnstrg( log_file(1:length_log) )
      fname = log_file(1:length_log)
      if((fname .ne. 'stdout') .and.
     *   (fname .ne. 'nolog')  .and.
     *   (fname .ne. 'screen'))
     *  call openfl(log_unit, log_file(1:length_log), openstat, 'null', 
     *   'none', 'none', 0)
c       call openfl(   iunit,                  ifile,    istat,  izero, 
c         ishr,   iform, irecl)
c    *  open (log_unit,file=log_file(1:length_log),status=openstat)
c --- use of status='scratch' is not standard and causes the files
c --- to be opened with "unique" names, which are uninformative.
c --- furthermore, it's sometimes useful to keep the temporary files
c --- when the program aborts.  therefore, you may wich to handle the
c --- "automatic" deletion another way (external to hypoellipse).
      write (out, *) 'root name = ', root(1:length_root)
c --- station scratch file number 1
      fname = root(1:length_root)//tmp1_type
c     call openfl(    iunit, ifile,     istat,  izero,   ishr,
c    *          iform, irecl)
      call openfl(tmp1_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 0)
c     open (tmp1_unit, file=root(1:length_root)//tmp1_type,
c    *      status='scratch')
c    *      form = 'noprint')
c --- station scratch file number 2
      fname = root(1:length_root)//tmp2_type
c     call openfl(    iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
      call openfl(tmp2_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 0)
c     open (tmp2_unit, file=root(1:length_root)//tmp2_type,
c    *      status='scratch')
c    *      form = 'noprint')
c --- temporary summary storage and uamag storage.  length must
c --- be 300 for uamag records.
      fname = root(1:length_root)//tmpsum_type
c     call openfl(       iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
      call openfl(tmpsum_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 300)
c     open (tmpsum_unit, file=root(1:length_root)//tmpsum_type,
c    *      status='scratch', recl=300)
c    *      form = 'noprint')
c --- temporary archive storage
      fname = root(1:length_root)//tmparc_type
c     call openfl(      iunit, ifile,     istat,  izero,   ishr,
c    *          iform, irecl)
      call openfl(tmparc_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 0)
c     open (tmparc_unit, file=root(1:length_root)//tmparc_type,
c    *      status='scratch')
c    *      form = 'noprint')
c --- temporary message storage
      fname = root(1:length_root)//tmpmess_type
c     call openfl(       iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
      call openfl(tmpmess_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 0)
c     open (tmpmess_unit, file=root(1:length_root)//tmpmess_type,
c    *      status='scratch')
c    *      form = 'noprint')
      return
      end
c end opfls
c output.for    []
      subroutine output(kp)
c---- output hypocenter information
      include 'params.inc' 
      parameter (ndly = 11)
      real bat2,bon2,latep,lonep,mag
      logical good, supout
c                 comp - z, n, or e component
      character*1 comp, dnstrg
c                 pha  - 3 letter phase description
      character*3 pha
      character*1 krmo, krm4, krm5, iprmk, iqs, iqd
      character erout*120, isorp*3
      character*133 fline, aline, bline, fmit*6, fmit1*6, fmit2*6, fmit3*24
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /amo/ tempg(npa), ntgap
      common /anox/ keyd(npa)
      character*4 ipro, ichec, evtype*1, evstat*1, root*256
      common /dbhip/ ipro, ichec, evtype, evstat, root
      common /dbiln/ ioldq
      character*1 iclass
      common /dbio/ iclass(0:4)
      common /dhio/ nedit,good
      common /dio/ rmsmx,presmx,sresmx,noutmx,nimx,iprun,semx
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      real*8 time1, time2
      common /hop/ time1,time2,nopu,notim
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /hpn/ wslope
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /idno/ ksel,ksort
      common /ihfgpq/ itest(100)
      common /ilotv/ elvdly(npa)
      common /in/ irmo,iryr,odmag
      character*1  magke
      common /in1/ magke
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /iot/ flt(2,nsn),thks(nsn)
      common /iox/ prr(nsn),iuses
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn), xmwt(nsn)
      common /logfil/ logfil
      common /it/ vpvs(lmax2),vpvsm(mmax+3),nttab
      common /omnfh/ dmin,dmin3,sminp
      common /onx/ kluse(npa), cluse(npa), peruse(npa), ampuse(npa)
      common /ox/ sysmag(npa), gndmot(npa) 
      common /obcfn/ ain(npa)
      common /ocfn/ adj,seh,lat1,bat2,lon1,bon2,igap
      character*1 isnla, isnlo, krm1, krm2
      common /ocfn1/ isnla, isnlo, krm1, krm2
      common /ofnv/ kmin
      character*1 nq
      common /ofnv1/ nq
      common /ohq/ gap, supout
      common /ohbl/ jav
      common /ofln/ sec
      common /olnx/ xmag(npa),fmag(npa),avxm,avfm,xmmed,fmmed
      common /on/ ix,iy,iz,ax1,axz
      character iqax*1
      common /on1/ iqax
      common /oqr/ y(4),kz,at(3),tl(3),pdrms,b(4)
      common /orz/ onf
      common /pfnoqv/ kdate,khrmn,khr
      character msym*1
      common /pfo/ msym(npa)
      common /pgoq/ s(npa)
      common /pgnoqv/ p(npa),jmin(npa)
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pgnov/ dt(npa)
      common /phoqn/ inst,knst
      common /pno/ lph,keyph(npa),nsum
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pnoqtx/ ldx(npa)
      character*4 krmp
      common /pnoqv/ krmp(npa)
      character*4 krms
      common /po/ krms(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /povx/ amx(npa)
      common /pox/ fmp(npa),prx(npa),icent1,icent2
      common /pt/ nlay(npa)
      common /qmost/ wt(npa),z
      common /qmost1/ lonep,ni,latep
      common /qgo/ org
      common /qgnotx/ delta(npa)
      common /qo/ dmax,ndec,adjsq,iph
      common /rbno/ idip(3),iaaz(3),a(3,3)
      common /rfgnoq/ se(4)
      common /ro/ yse,seorg,phi
      common /rioq/ been,damp,dmpinc,igo
      common /rob/ v(4,4),noaxp
      common /rq/ xmean(4),avwt,aveaj
      common /tmost/ x(4,npa)
      common /tonxb/ t(npa),fms(npa)
      common /xo/ nm,sdxm,nf,sdfm
      character*1 kodsym
      common /xo1/ kodsym(npa)
      common /xfmno/ mag
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      dimension demp(npa)
      dimension pruse(npa),sruse(npa),keyi(npa)
      dimension vv(2,2), ael(3), vec(2,2), eval(2), key(npa)
      data dum/1.0/
      if(iprn .ge. 5) write(punt, '(a)') 'begin subroutine output'
      iqax = ' '
      noaxp = 0
      rpd = 1.74533e-2
c---- initialize some remarks and calculate magnitudes if necessary
      krm1 = ' '
      krmo = ' '
c---- onf is sum of p and s weights. if onf = 0. put * next to origin time
      if(onf .eq. 0.) krmo = '*'
      krm2 = ' '
c---- kz is the number of fixed component in regress
      if(kz .eq. 3) krm2 = '*'
c---- define ix, iy, and iz in order of dip
      ix = 1
      iy = 2
      iz = 3
      if(idip(1) .le. idip(2)) goto 600
      ix = 2
      iy = 1
  600 if(idip(iy) .le. idip(3)) goto 610
      iz = iy
      iy = 3
  610 if(idip(ix) .le. idip(iy)) goto 620
      k = ix
      ix = iy
      iy = k
  620 continue
c---- convert lat and long to degrees and minutes
c---- convert lat and long to degrees and minutes
      if(iprn .ge. 5) then
        write(punt, '(a)') ' convert lat and long to deg & min'
        write(punt, *) ' latep, lonep = ', latep, lonep   
      endif 
      call unfold2(latep,lonep,lat1,isnla,bat2,lon1,isnlo,bon2)
      if(iprn .ge. 5) write(punt, *) lat1, isnla, bat2, lon1, 
     *                               isnlo, bon2 
c---- calculate time
      khr = ihr
      kmin = lbastm
      sec = org
      kkdate = kdate
c     write(punt, '(a, 2i10, f10.2, 2i10)') 
c    *  'ihr, lbastm, org, kdate, kkdate',
c    *  ihr, lbastm, org, kdate, kkdate
      call tshift(kkdate,khr,kmin,sec,dum)
      adj = sqrt(adjsq)
      if((se(ix) .eq. blank) .or. (se(iy) .eq. blank)) then
        eqh = 10.0
      else
        eqh=sqrt(se(ix)**2+se(iy)**2)
      endif
c---- calculate gap
  431 j=0
      do 10 i = 1,nrp
        if (wt(i) .gt. 0.) then
c         continue on because there is p data
        else
c         check for s data
          if (ldx(i) .eq. 0) then
c           no s data
            goto 10
          else
c           if the s data has zero weight, skip it
            if (wt(ldx(i)) .eq. 0.) goto 10
          endif
        endif
        j=j+1
        tempg(j)=az(i)
   10 continue
      ntgap = j
      gap = 360.
      if(j .lt. 2) goto 22
      call sort(tempg,key,j)
      gap=tempg(1)+360.-tempg(j)
      do 20 i=2,j
      dtemp=tempg(i)-tempg(i-1)
      if(dtemp .gt. gap) gap=dtemp
   20 continue
   22 igap = gap + 0.5001
c---- calculate minimum distance
c---- sruse and pruse are arrays of s and p residuals used in solution
c---- nwtout is number of readings weighted out by the program
      iminp = 1
      dminp = 999.
      imins = 1
      dmins = 999.
      sruse(1) = 0.0
      pruse(1) = 0.0
      nwtout = 0
      iwt = 0
      ipwt = 0
      iswt = 0
      do 26 i = 1,nrp
        ianywt = 0
        kk = ldx(i)
        if(wt(i) .eq. 0.0) goto 23
        ianywt = 1
        ipwt = ipwt + 1
        if(delta(i) .lt. dminp) then
          dminp = delta(i)
          iminp = i
        endif
        pruse(ipwt) = abs(x(4,i))
   23   if (kwr(i) .ne. ' ' .and. dnstrg(kwr(i)) .ne. 'g') 
     *   nwtout = nwtout + 1
        if(kk .eq. 0) goto 24
        if (kwr(kk) .ne. ' ' .and. dnstrg(kwr(i)) .ne. 'g') 
     *   nwtout = nwtout + 1
        if(wt(kk) .eq. 0.0)  goto 24
        ianywt = 1
        iswt = iswt + 1
        if(delta(i) .lt. dmins) then
          dmins = delta(i)
          imins = i
        endif
        sruse(iswt) = abs(x(4,kk))
   24   if(ianywt .eq. 0) goto 26
        iwt = iwt + 1
        demp(iwt) = delta(i)
   26 continue
      dmin = dminp
      if(dmins .lt. dmin) dmin = dmins
      dmin3=dmin
      if(iwt .gt. 1) call sort(demp,key,iwt)
      if(iwt .ge. 2) dmin3 = demp(2)
      if(iwt .ge. 3) dmin3 = demp(3)
c---- compute s minus p time for closest station used in solution
      sminp = blank
      if((ipwt .eq. 0) .or. (iswt .eq. 0)) goto 950
c---- dminp/dmins is distance to closest station with p/s used
c---- iminp/imins is index of the closest stations with p/s
      if(msta(iminp) .ne. msta(imins)) goto 950
      sminp = s(imins) - p(iminp)
      if((sminp .gt. 99.9) .or. (sminp .lt. 0.)) sminp = 99.9
  950 if(ipwt .eq. 0) ipwt = 1
      if(iswt .eq. 0) iswt = 1
c---- calculate magnitude
      mag = blank
      if((iprn .ge. 2) .or. (kp .eq. 1)) call xfmags
      rmag = mag
c---- calculate q, iqs, and iqd
      ofd=z
      tfd=2.*z
      if(ofd .lt. 5.) ofd=5.
      if(tfd .lt. 10.) tfd=10.
      js=4
      if((rms.lt.0.50).and.(eqh.le.5.0)) js=3
      if(se(iz) .eq. blank) goto 28
      if((rms.lt.0.30).and.(eqh.le.2.5).and.(se(iz).le.5.0)) js=2
      if((rms.lt.0.15).and.(eqh.le.1.0).and.(se(iz).le.2.0)) js=1
   28 jd = 4
      if(nrwt .lt. 6) goto 30
      if((gap.le.180.).and.(dmin.le.50.)) jd=3
      if((gap.le.135.).and.(dmin.le.tfd)) jd=2
      if((gap.le. 90.).and.(dmin.le.ofd)) jd=1
   30 jav=(js+jd+1)/2
      nq=iclass(jav)
      iqs=iclass(js)
      iqd=iclass(jd)
      idmin=dmin + .5
c---- write heading and check if eq is out of order
      time2=1.d+0*sec+1.d+02*kmin+1.d+04*khr+1.d+06*kkdate
      if(iprn .lt. 0) goto 720
      if((iprn .eq. 0) .or. (inst .eq. 9)) goto 52
      if(ni .ne. 1) goto 60
      if(ndec .ge. 1) goto 60
   52 continue
      if((icent2 .eq. icent1 .and. time2 .lt. time1 - 20.d0) .or.
     *  (icent2 .lt. icent1)) then
        write(punt,54) icent2, kkdate, khr, kmin, sec
        write(logfil,54) icent2, kkdate, khr, kmin, sec
   54   format(' xxxxx this event is out of order xxxxx', /,
     *  i2, i6.6, 1x, i2, ':', i2, 1x, f6.2)
      endif
c---- write step output
   60 if(iprn .eq. 0) goto 720
      if(iph .eq. 1) goto 62
c---- step line  output
      write(punt,61)
   61 format(/,84x,
     *'( adjustments )(adjst.  taken)', /,
     *'  i  lat long depth    rms   prms    damp     no ',
     *'  phi  --------eigenvalues---------',
     *' dlat dlon   dz dlat dlon   dz')
      if(iprn .eq. 1) iph=1
c
   62 continue
      aline = ' '
c      write(aline, 162) ni,bat2,bon2,z-test(8),krm2,rms,nrwt
c  162 format(1x ,i2, f5.1, f5.1, f6.1, a1, f7.4, i3)
      write(aline, 162) ni,bat2,bon2,z-test(8),krm2
  162 format(1x ,i2, f5.1, f5.1, f6.1, a1)
c
      call formit(rms, rrms, fmit, 6, 1)
      write(aline(22:27), fmit) rrms

      if (pdrms .ne. blank) then
        call formit(pdrms, rpdrms, fmit, 6, 1)
        write(aline(29:34), fmit) rpdrms
      endif
c
      write(aline(36:42), '(e7.2)') damp
      write(aline(46:84), '(i3, 1x, f5.1, 3(1x, e9.4))') 
     *  nrwt, phi, a(1,1), a(2,2), a(3,3)
c
      if (b(2) .ne. blank) then
        call formit(b(2), rb, fmit, 4, 1)
        write(aline(86:89), fmit) rb
      endif
c
      if (b(1) .ne. blank) then
        call formit(b(1), rb, fmit, 4, 1)
        write(aline(91:94), fmit) rb
      endif
c
      if (b(3) .ne. blank) then
        call formit(b(3), rb, fmit, 4, 1)
        write(aline(96:99), fmit) rb
      endif
c
      call formit(y(2), rb, fmit, 4, 1)
      write(aline(101:104), fmit) rb
      call formit(y(1), rb, fmit, 4, 1)
      write(aline(106:109), fmit) rb
      call formit(y(3), rb, fmit, 4, 1)
      write(aline(111:114), fmit) rb

      if( (ni .eq. 1) .or. (ni .eq. itest(37)) )
     *  write(punt, 64) lat1, lon1
   64 format(3x, 2i5)
c
c     step output
      write(punt, '(a132)') aline
c
      if((test(41) .eq. 1.0) .and. (iabs(ipun) .eq. 1))
     *call npunch('final')
      if((kp .eq. 0) .and. (iprn .lt. 3)) goto 100
c---- calculate single variable standard deviations
c---- first calculate shadow on surface
c-------- vv(1,1)*x**2 + vv(1,2)*x*y + vv(2,2)*y**2 = 1
  720 if(v(3,3) .lt. 0.000009) v(3,3) = 0.000009
      vv(1, 1) = (v(1,1)-v(1,3)**2/v(3,3))
      vv(1, 2) =    (v(1,2)-v(1,3)*v(2,3)/v(3,3))
      vv(2, 1) = vv(1, 2)
      vv(2, 2) = (v(2,2)-v(2,3)**2/v(3,3))
c---- compute principal axis
c---- find eigenvalues [eval(2)] and eigenvectors [vec(2,2)] for the
c     upper left nxn portion of vv(2, 2)
      call eigen1(ael, 2, 2, 2, vec, eval, vv, 0.0)
c
      if(eval(2) .lt. 0.000009) then
        ax1 = 99.
      else
        ax1 = yse/sqrt(eval(2))
        if(ax1 .gt. 99.) ax1 = 99.
      endif
c
      if(eval(1) .lt. 0.000009) then
        ax2 = 99.
      else
        ax2 = yse/sqrt(eval(1))
        if(ax2 .gt. 99.) ax2 = 99.
      endif
c
      aze2 = atan2(-vec(1,1),vec(2,1))/rpd
      aze1 = atan2(-vec(1,2),vec(2,2))/rpd
c
c---- compute maximum in z (standard error of z)
      if((v(1,1) .lt. 0.000009) .or. (nrwt .le. 3)) then
        axz = 99.
      else
        dnom = v(2,2) - v(1,2)*v(1,2)/v(1,1)
        if(dnom .lt. 0.000009) then
          axz = 99.
        else
          prob =  v(3,3) - v(1,3)**2/v(1,1)
     *    -( v(2,3) - v(1,3)*v(1,2)/v(1,1) )**2/dnom
          if(prob .lt. 0.000009) then
            axz = 99.
          else
            axz = yse/sqrt(prob)
            if(axz .gt. 99.) axz = 99.
          endif
        endif
      endif
c---- calculate quality based upon ax1 and ax2
      jq = 4
      bigst = axz
      if(ax1 .gt. axz) bigst = ax1
      if(bigst .le. 5.35) jq = 3
      if(bigst .le. 2.67) jq = 2
      if(bigst .le. 1.34) jq = 1
      iqax = iclass(jq)
      if(ioldq .eq. 0) jav = jq
      if(iprn .lt. 0) return
      if((ksel.ne.1) .and. ((nedit.eq.1).or.(nedit.eq.2))) goto 70

c write out az, dip, step and se for the principal directions
      write(punt, '(9x, a)') 
     * '-az/dp--step---se =az/dp==step===se -az/dp--step---se' 
      aline = ' '
      write(aline(10:15), '(i3, 1h/ ,i2)') iaaz(ix),idip(ix)
c
      if (at(ix) .ne. blank) then
        call formit(at(ix), rat, fmit, 5, 1)
        write(aline(17:21), fmit) rat
        call formit(se(ix), rse, fmit, 4, 1)
        write(aline(23:26), fmit) rse
      endif
c
      write(aline(28:33), '(i3, 1h/ ,i2)') iaaz(iy),idip(iy)
c
      if (at(iy) .ne. blank) then
        call formit(at(iy), rat, fmit, 5, 1)
        write(aline(35:39), fmit) rat
        call formit(se(iy), rse, fmit, 4, 1)
        write(aline(41:44), fmit) rse
      endif
c
      write(aline(46:51), '(i3, 1h/ ,i2)') iaaz(iz),idip(iz)
c
      if (at(iz) .ne. blank) then
        call formit(at(iz), rat, fmit, 5, 1)
        write(aline(53:57), fmit) rat
        call formit(se(iz), rse, fmit, 4, 1)
        write(aline(59:62), fmit) rse
      endif
      write(punt, '(a)') aline

c write out comment records
      do 530 i = 1,lph
        if(keyph(i) .eq. -1) write(punt,520) phcard(i)(1:115)
  520   format(1x,a)
  530 continue

      if(iprn .ge. 0)
     * write(punt,65) ax2,ax1,axz,iqax,aze2,aze1
   65 format(/1x,'horizontal and vertical single variable standard',
     *   ' deviations (68% - one degree of freedom; max 99 km)'/,
     *  7x,6hseh = ,f6.2,13x,6hseh = ,f6.2,13x,6hsez = ,f6.2,
     * 13h   quality = ,a1/7x,6haz  = ,f6.0,13x,6haz  = ,f6.0/)
      write(punt,78) seorg,ni,dmax,seq
   78 format(' se of orig = ',f6.2,'; # of iterations = ',
     *       i3,'; dmax = ',f10.2,'; sequence number = ',a5)
      if(sminp .eq. blank) then
        write(punt, 975) evtype, evstat
  975   format(' event type = "', a1, '"',
     *           '; processing status = "', a1, '"', /,
     *           ' closest station did not use both p and s')
      else
        write(punt, 980) evtype, evstat, sminp
  980   format(' event type = "', a1, '"',
     *           '; processing status = "', a1, '"', /
     *    ' s minus p interval for closest station = ', f10.2)
      endif
c
c---- check for debug event
   70 continue
      if(nedit .ne. 0) then
        nopu = 1
        good = .true.
        call sort(pruse,keyi,ipwt)
        call sort(sruse,keyi,iswt)
        if(bigst .gt. semx) goto 107
        if(ni .gt. nimx) goto 107
        if(rms .gt. rmsmx) goto 107
        if(sruse(iswt) .gt. sresmx) goto 107
        if(pruse(ipwt) .gt. presmx) goto 107
        if(nwtout .gt. noutmx) goto 107
c----   event is good
        if(iabs(nedit) .eq. 1) then
          write(punt,74) iqax
   74     format('  quality based on standard errors is ',a1/)
          goto 73
        endif
        if(iabs(nedit) .eq. 3) goto 73
        noaxp = 1
        return
  107   good = .false.
        write(punt,76) ni,rms,pruse(ipwt),sruse(iswt),nwtout,bigst
   76   format(1h0,' d e b u g  e v e n t     ni   rms presmx sresmx',
     *  ' nwtout  bigst   ',/,
     *  24x,                              i5, f6.1,  f7.1,  f7.1,
     *  i7,  f7.1)
        if(nedit .lt. 0) nopu=0
        if(iabs(nedit) .eq. 3) goto 73
      endif
   73 continue
c
c---- check for preferred magnitude
      if((magke .ne. ' ') .and. 
     * (dnstrg(magke) .ne. 'x') .and.
     * (dnstrg(magke) .ne. 'f') .and. 
     * (dnstrg(magke) .ne. 'a')) then
        write(punt,77) odmag,magke
   77   format('  preferred magnitude on summary record will be: ',
     *  f5.2,1x,a1)
      endif
c
c---- set up final output line
      write(punt,75)
   75 format(/'    date    origin      lat      long    depth    mag',
     * ' no d1 gap d  rms    avwt   se')
      fline = ' '
      write(fline, 71) icent2, kkdate, krmo, khr, kmin, sec, 
     *  lat1, isnla, bat2, lon1, isnlo, bon2, krm1, z-test(8), krm2
      jnst = knst*10 + inst
   71 format(1x, i2, i6.6, a1, 2i2, f6.2, i3, a1, f5.2, i4, a1,
     *       f5.2, a1, f6.2, a1)
c---- add magnitude
      if (rmag .ne. blank) then
        call formit(mag, rmag, fmit, 5, 1)
        write(fline(49:53), fmit) rmag
      endif
c----- add up through rms
      write(fline(54:84), 82) nrwt, idmin, igap, kno, rms, avwt, yse
   82 format(2i3, i4, i2, f7.4, f6.2, f6.2)
      write(punt, '(a)') fline(1:84)
      fline = ' '
c
      write(punt, 821)
821   format(/'    seh  sez q sqd  adj in nr   avr  aar nm',
     * ' avxm mdxm sdxm nf avfm mdfm sdfm   vpvs')
      write(fline(1:29), 822) ax1, axz, nq, iqs, iqd, adj, jnst, nr
822   format( 1x, f6.1, f5.1, 3(1x,a1), f5.2, 2i3)
c----- add avr, aar, and nm
      call formit(avr, avrlm, fmit1, 5, 1)
      call formit(aar, aarlm, fmit2, 4, 1)
      fmit3 = fmit1(1:5)//',1x,'//fmit2(2:5)//',i3)'
      write(fline(31:44), fmit3) avrlm, aarlm, nm
c----- add average xmag
      if (avxm .ne. blank) then
        call formit(avxm, ravxm, fmit, 4, 1)
        write(fline(45:48), fmit) ravxm
      endif
c----- add median xmag
      if (xmmed .ne. blank) then
        call formit(xmmed, ravxm, fmit, 4, 1)
        write(fline(50:53), fmit) ravxm
      endif
c----- add two more
      write(fline(54:62), 83) sdxm,nf
   83 format(f5.1, i3)
c----- add average fmag
      if (avfm .ne. blank) then
        call formit(avfm, ravfm, fmit, 4, 1)
        write(fline(63:66), fmit)ravfm
      endif
c----- add median fmag
      if (fmmed .ne. blank) then
        call formit(fmmed, ravfm, fmit, 4, 1)
        write(fline(68:71), fmit)ravfm
      endif
c----- add rest
      write(fline(72:83), 84) sdfm, wslope
   84 format(f5.1, f7.3)
c
      write(punt, '(a)') fline(1:83)
c
c
c---- write out warning messages
      rewind(16)
      read(16, 97) erout
   96 read(16, 97, end=98) erout
        if(erout(1:3) .eq. 'end') goto 98
        write(punt, 97) erout
        write(logfil, 97) erout
   97   format(a)
      goto 96
   98 continue
c
c for now, remove short-circuit of detailed printout
c     if(nedit .eq. 0) goto 100
c     if( .not. good) goto 108
c     if(iprn .ge. 1) goto 108
c     return
c
  100 if(kp .eq. 1) goto 108
      if(iprn .le. 1) return
  108 isorp = 'fmp'
      if(iuses .eq. 1) isorp = 'fms'
      write(punt,110)
  110 format (/'                      -- travel times and delays --', /,
     *'  stn c pha remk p p-sec s-sec resid  std-er   dist  azm ain',
     *'    tc c vthk  ttob-ttcal-dlay-edly=resid rmk stn pha sources')
czzz
c***     1         2         3         4         5         6         7         8
c***5678901234567890123456789012345678901234567890123456789012345678901234567890
c stn c pha remk p p-sec s-sec resid  std-er   dist  azm ain    tc c vthk  ttob-
cbrse     1 ipu3 d199.25       -0.99 * 1.011  199.3  241 180  -.26 1 4.00 49.32
cbrse   s 1  s 2         49.25 -0.99 r 1.011             180  -.26 1 4.00 49.32
c spu     2 ip+9 -  3.22                      551.1  123  82  -.26 1 2.00 49.32
c spu   s 2  s 2         13.22                                -.26 1 2.00 49.32
c spu   smp                     1.55   1.222                              14.00
c
      if(nrp .eq. 0) return
      do 500 i=1,nrp
      aline = ' '
      k=i
      if(ksort .eq. 0) k = keyd(i)
      kk = ldx(k)
c
c set up component
c     if(dnstrg(krmp(k)(2:2)) .eq. 'p') then
c       comp = 'z'
c       comp = ' '
c     else
c       comp = krmp(k)(2:2)
c     endif
      comp = msta(k)(5:5)
c set up p-phase identifier
c     pha = 'p  '
      pha = '   '
      if(nlay(k) .ne. 0) then
        write(pha(3:3), '(i1)') nlay(k)
      endif
c
c      write(aline(1:24), 91) msta(k),  comp,   pha, krmp(k), msym(k),
c     1 p(k)
      write(aline(1:24), 91) msta(k)(1:4),  comp,   pha, krmp(k), 
     *  msym(k)
91    format(                  1x,a4, 1x,a1, 1x,a3,   1x,a4,   1x,a1)
c add p arrival time
      aline(20:24) = phcard(keyphi(k))(20:24)
c
      pres = x(4,k)
      apres = abs(pres)
      iprmk = kwr(k)
      if(iprun .ne. 1) then
        if(wt(k) .gt. 0.) then
c fix up p residuals for routine processing
          if(iprmk .eq. ' ') then
            if((apres.gt.0.6).and.(i.le.5)) iprmk = '*'
            if((apres.gt.0.9).and.(delta(k).lt.150.)) iprmk = '*'
            if((apres.gt.1.5).and.(delta(k).lt.350.)) iprmk = '*'
          endif
        endif
        if(apres .lt. 2.25) then
          l = (apres + 0.25)/0.5
          pres = l*0.5
        endif
      endif
      if(pres .gt. 999.99) pres = 999.99
      if(pres .lt. -99.99) pres = -99.99
c add p-residual
      if(wt(k) .eq. 0.0) then
        write(aline(31:44), '(f6.2, 1x, a1, a)') pres, iprmk, ' -----'
      else
        setmp = yse/sqrt(wt(k))
        if(setmp .gt. 99.99) setmp = 99.99
        write(aline(31:44), '(f6.2, 1x, a1, f6.2)') pres, iprmk, setmp
      endif
c
      write(aline(45:52), 93) delta(k), iqdo(k)
93    format(                     f7.1,      a1)
c
      call formf(az(k), iaz, 3, 0)
      write(aline(53:56), '(i4)') iaz
c
      call formf(ain(k), iain, 4, 0)
      write(aline(57:60), '(i4)') iain
c add time correction
      if (dt(k) .ne. 0.) then
        dtk = dt(k)
        call formit(dtk, dtk, fmit, 5, 1)
        write(aline(62:66), fmit) dtk
      endif
c add model
      write(aline(67:68), '(i2)') model(k)
c leave one blank
c add variable layer thickness
      if (thks(k) .ne. blank) then
        call formit(thks(k), rthks, fmit, 4, 1)
        write(aline(70:73), fmit) rthks
      endif
c add p-travel time observed
      tpk = tp(k) - org
      if(tpk .lt. -3500.) tpk = tpk + 3600.
      write(aline(74:85), 94) tpk, t(k)
94    format(2f6.2, i2)
c leave one blank
c add p-delay
      kji=kdx(k)
      if (dly(kno, kji) .ne. 0.) then
        call formit(dly(kno,kji), dlyk, fmit, 4, 1)
        write(aline(87:90), fmit) dlyk
      endif
c leave one blank
c add elevation delay
      if (elvdly(k) .ne. 0.) then
        call formit(elvdly(k), eldly, fmit, 4, 1)
        write(aline(92:95), fmit) eldly
      endif
c repeate p-residual and add remark
      write(aline(96:104), '(f6.2, 1x, a2)') pres, krm(k)
c leave one blank
c repeate station name
      aline(106:109) = msta(k)(1:4)
c leave one blank
c repeate phase type
      aline(111:113) = aline(9:11)
c leave two blanks
c give sources and number of hops from phase record
      aline(116:121) = phcard(keyphi(k))(105:110)
c
c aline is now set up correctly for a normal p phase
      if(ldx(k) .eq. 0) then
        if(ksmp(k) .eq. 1) then
c no s data - write p record and go on to next phase
          write(punt, '(a)') aline(1:121)
          goto 500
        else
c s minus p data -- remove residual and std-er
c set up bline as a revised p record and write out
98764     bline = aline
          bline(25:45) = ' '
c compute the p-residual
          pres = tp(k) - t(k) - org - dly(kno, kji) - elvdly(k)
          if(pres .gt. 999.99) pres = 999.99
          if(pres .lt. -99.99) pres = -99.99
          write(bline(96:101), '(f6.2)') pres
c write out the p line for an s-p
          write(punt, '(a)') bline(1:121)
c
c next set up bline as an s record
          bline(9:9) = 's'
          write(bline(13:16), '(a4)') krms(k)
c remove p seconds
          bline(20:24) = ' '
c add s seconds
          bline(26:30) = phcard(keyphi(k))(32:36)
c remove dist, azm, and ain
          bline(47:60) = ' '
c add observed and computed travel times
          tsk = ts(k) - org
          if(tsk .lt. -3500.) tsk = tsk + 3600.
          write(bline(74:86), '(2f6.2)') tsk, vpvsm(model(k))*t(k)
c add s-delay
          if (sdly(kno, kji) .ne. 0.) then
            call formit(sdly(kno,kji), sdlyk, fmit, 4, 1)
            write(bline(87:90), fmit) sdlyk
          else
            bline(87:90) = ' '
          endif
c add elevation delay
          if (elvdly(k) .ne. 0.) then
            call formit(vpvsm(model(k))*elvdly(k), eldly, fmit, 4, 1)
            write(bline(92:95), fmit) eldly
          else
            bline(92:95) = '    '
          endif
c compute the s-residual
98765     sres = ts(k) - org - vpvsm(model(k))*(t(k) + elvdly(k))
     *    - sdly(kno, kji)
          if(sres .gt. 999.99) sres = 999.99
          if(sres .lt. -99.99) sres = -99.99
          write(bline(96:101), '(f6.2)') sres
          bline(111:111) = 's'
          write(punt, '(a)') bline(1:113)
c
c finally set up aline as the s minus p record
          aline(9:30) = 'smp'
          aline(45:73) = ' '
c add observed and computed s-p
          csmp = (vpvsm(model(k))-1.)*(t(k) + elvdly(k))
     *    - dly(kno, kji) + sdly(kno, kji)
          write(aline(74:85), '(2f6.2)') ts(k)-tp(k), csmp
          aline(86:104) = ' '
c repeate phase type
          aline(111:113) = aline(9:11)
          write(punt, '(a)') aline(1:113)
        endif
      else
c
c this is the case of a normal p and s
c first write p record
          write(punt, '(a)') aline(1:121)
c
c next convert aline into an s record
        aline(9:9) = 's'
        aline(13:16) = krms(k)
c remove corrected first motion and p-res
        aline(18:24) = ' '
c add s seconds
        aline(25:30) = phcard(keyphi(k))(32:36)
        sres = x(4,kk)
        krm5 = kwr(kk)
        if(iprun .ne. 1) then
c fix up s residuals for routine processing
          asres = abs(sres)
          if(wt(kk) .gt. 0.0) then
            if(krm5 .eq. ' ') then
              if((asres.gt.0.9).and.(i.le.5)) krm5 = '*'
              if((asres.gt.1.5).and.(delta(k).lt.150.)) krm5 = '*'
              if((asres.gt.2.0).and.(delta(k).lt.350.)) krm5 = '*'
            endif
          endif
          if(asres .lt. 2.25) then
            l = (asres + 0.25)/0.5
            sres = l*0.5
          endif
        endif
        if(sres .gt. 999.99) sres = 999.99
        if(sres .lt. -99.99) sres = -99.99
        if(wt(kk) .eq. 0.0) then
          write(aline(31:44), '(f6.2, 1x, a1, a)') sres, krm5, ' -----'
        else
          setmp = yse/sqrt(wt(kk))
          if(setmp .gt. 99.99) setmp = 99.99
          write(aline(31:44), '(f6.2, 1x, a1, f6.2)') sres, krm5, setmp
        endif
c remove dist and azm
        aline(47:56) = ' '
        call formf(ain(kk), iain, 4, 0)
        write(aline(57:60), '(i4)') iain
        write(aline(67:68), '(i2)') model(kk)
c add observed and computed travel times
        tsk = ts(k) - org
        if(tsk .lt. -3500.) tsk = tsk + 3600.
        write(aline(74:86), '(2f6.2)') tsk, t(kk)
c add s-delay
        if (sdly(kno, kji) .ne. 0.) then
          call formit(sdly(kno,kji), sdlyk, fmit, 4, 1)
          write(aline(87:90), fmit) sdlyk
        else
          aline(87:90) = ' '
        endif
c add elevation delay
        if (elvdly(kk) .ne. 0.) then
          call formit(elvdly(kk), eldly, fmit, 4, 1)
          write(aline(92:95), fmit) eldly
        else
          aline(92:95) = '    '
        endif
c repeate s-residual and leave remark
        write(aline(96:101), '(f6.2)') sres
c repeate phase type
        aline(111:113) = aline(9:11)
        write(punt, '(a)') aline(1:113)
      endif
500   continue
c
c loop through again for magnitude data
c
      write(punt, 550)
550   format(/,
     * '                                     -- magnitude data --',/,
     * '  stn c source sys    c10   amx gr ink     amf    per ',
     * '  unit/mm  gnd mot u xmgc xmag  fmp fmag')
      do 900 i = 1, nrp
        aline = ' '
        k=i
        if(ksort .eq. 0) k = keyd(i)
        kji=kdx(k)
        if (amx(k) .ne. 0.) then
          write(aline(16:17), '(i2)') kluse(k)
          aline(28:31) = phcard(keyphi(k))(44:47)
c add corrected amplitude
          write(aline(39:47), '(f9.0)') ampuse(k)
        endif
        if (cluse(k) .gt. 0.) then
          write(aline(18:25), '(f8.2)') cluse(k)
	endif
	if ((sysmag(k) .ne. blank) .and. (gndmot(k) .ne. blank)) then
          write(aline(55:74), '(1pe10.4, 1x, 1pe9.3)') 
     *    sysmag(k), gndmot(k)
        endif
c a1vco gain range state 0, 1, or 2
        aline(34:34) = phcard(keyphi(k))(62:62)
c semens playback state 0 or 1
        aline(37:37) = phcard(keyphi(k))(61:61)
c add xmag
        if (xmag(k) .ne. blank) then
          write(aline(75:80), '(f5.2, 1x)') xmgc(kji)
          call formit(xmag(k), rxmag, fmit, 4, 1)
          write(aline(81:84), fmit) rxmag
        endif
c flag deviant xmags
        if(xmag(k) .ne. blank) then
	  if(xmwt(kji) .eq. 0) then
c flag excluded xmags
c 	    aline(65:65) = 'e'
	    aline(85:85) = 'e'
	  else if(abs(xmag(k) - avxm) .ge. 0.5) then
c flag deviant xmags 
c           aline(65:65) = '*'
            aline(85:85) = '*'
	  endif
        endif
c add f-p
        rfmp = fmp(k)
        if(iuses .eq. 1) rfmp = fms(k)
        if (rfmp .ne. blank) then
          call riorbk(rfmp, ifmp, fmit, 4, 0)
c         write(aline(66:69), fmit) ifmp
          write(aline(86:89), fmit) ifmp
        endif
c leave 2 blanks
c add fmag
        rfmag = fmag(k)
        if (fmag(k) .ne. blank) then
          call formit(fmag(k), rfmag, fmit, 4, 1)
c         write(aline(71:74), fmit) rfmag
          write(aline(91:94), fmit) rfmag
        endif
c
        if(dnstrg(kodsym(k)) .eq. 's') then
c fmp too short with respect to s-p, so fmag not computed
          krm4 = kodsym(k)
        else
          krm4 = ' '
          if(fmag(k) .ne. blank) then
            if(fmwt(kji) .eq. 0) then
c flag excluded fmags
              krm4 = 'e'
            else if(abs(fmag(k) - avfm) .ge. 0.5) then
c flag deviant fmags with krm4
              krm4 = '*'
            endif
          endif
        endif
c       aline(75:75) = krm4
        aline(95:95) = krm4
	if(aline(1:33) .ne. ' ')
     *    write(aline(49:53), '(f5.2)') peruse(k)
        if((aline(1:33) .ne. ' ') .or. 
     *    (aline(38:133) .ne. ' ')) then
c set up component
c         if(dnstrg(krmp(k)(2:2)) .eq. 'p') then
c           comp = 'z'
c           comp = ' '
c         else
c           comp = krmp(k)(2:2)
c         endif
	  comp = msta(k)(5:5)
          aline(2:5) = msta(k)(1:4)
          aline(7:7) = comp
c add amplitude source
          aline(11:11) = phcard(keyphi(k))(108:108)
c         write(punt, '(a)') aline(1:75)
          write(punt, '(a)') aline(1:95)
        endif
900   continue
      return
      end
c end output
c phasin.for    []
      subroutine phasin
c get phase records for one earthquake and initialize related arrays
      include 'params.inc' 
      parameter (ndly = 11)
      character*1 poldev, pdev, comp
      character*1 isnla, isnlo, jsnla, jsnlo
      character*4 mamaz, mamla, mamor*6, fmit, rshft, dnstrg*5
      character*4 malaz, malla, malor*6
c       poldev specifies device from which p-polarity was obtained
c       pdev specifies device from which p-reading was made -
c            must have same telemetry delay as s-reading, because
c            the same telemetry delay is assumed for both
c       device codes currently used by usgs/uagi are:
c        v,*,1,4 - uags film viewed at 20x
c              v - viewer, geotech or kodak
c              * - assumed to be 20x film
c              1 - one-film digitizer
c              4 - four-film digitizer
c          %,a,f - uagi film viewed at 20x
c              % - assumed to be uagi 20x film
c              a - readings provided to usgs by uagi
c              f - uagi films read by usgs at uagi
c              w - atwc film viewed at 20x
c          d,j,x - daq or dan digital data
c        e,2 - usgs digitized from magnetic tape
c              e - elipse processing
c              2 - cusp processing
c        s - usgs siemens playback from magnetic tape
c        h - usgs helicorder
c        r - uagi helicorder
c        5 - five-day recorder
c        m - sma1 film recorder
c        l - usgs elog
c        b - published bulletin
c        c - unpublished canadian data (eg. tape)
c        t - atwc teletype (corrected for telemetry delay)
c        n - neis (corrected for telemetry delay)
c        p,o,g,k,i,u - pc recorders
c
      character*1 revers
      character seqins*5
      logical eoff, setmast, scatnow
      real lat,lon,latr,lonr,mag
      character*4 iahead*60, msta*5, msta4*4, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      character*4 ipro, ichec, evtype*1, evstat*1, root*256
      common /dbhip/ ipro, ichec, evtype, evstat, root
      character*1 bksrc
      common /dipu/ ipkdly, igsum, bksrc
      common /dmost/ ipun,ivlr,blank
      common /dph/ noswt, eoff
      common /gmost/ az(npa)
      real*8 time1, time2
      common /hop/ time1,time2,nopu,notim
      common /hpn/ wslope
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /iclmpq/ lat(nsn),lon(nsn)
      common /imost/ test(100)
      common /ihfgpq/ itest(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /ilmpu/ ns
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /ilv/ c(nsn),e(nsn)
      common /iox/ prr(nsn),iuses
      common /logfil/ logfil
      common /in/ irmo,iryr,odmag
      character*1  magke
      common /in1/ magke
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn), xmwt(nsn)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
      common /ip1/ rsew(4)
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /olnx/ xmag(npa),fmag(npa),avxm,avfm,xmmed,fmmed
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      character msym*1
      common /pfo/ msym(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pgnov/ dt(npa)
      common /pgoq/ s(npa)
      common /pgnoqv/ p(npa),jmin(npa)
      common /pgqv/ w(npa)
      common /ph/ nfirst
      common /phoqn/ inst,knst
      common /pm/ jdx(nsn)
      common /pn/ itpused(npa)
      common /pnl/ ww(npa)
      common /pno/ lph,keyph(npa),nsum
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      common /pnoqtx/ ldx(npa)
      character*4 krmp
      common /pnoqv/ krmp(npa)
      character*4 krms
      common /po/ krms(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /povx/ amx(npa)
      common /pox/ fmp(npa),prx(npa),icent1,icent2
      common /pt/ nlay(npa)
      common /pqt/ near
      common /reloc/ irelo, nreloc
      common /qmost/ wt(npa),z
      common /xfmno/ mag
      dimension ws(npa),wws(npa)
      character*1 ifmsm(8)
      character*36 scatph(nsn)
      data ifmsm/'c','d','+','-','d','c','-','+'/
      data scatp/0./, scats/0./, amplif/1.0/, fract/-1./
      data scatnow/.false./, icent2/0/
      if(icent2 .eq. 0) icent2 = itest(55)
      icent1 = icent2
      rewind (16)
      if (iprn .ge. 5) write(punt, '(a)') ' begin phasin'
      kdate = 0
      odmag = -10.
      setmast = .false.
      write(16, 19)
19    format('$beginning of phasin error file')
192   seq = '     '
      magke = ' '
      near = 1
      nfirst = 0
      iskip = 0
      pmin = 9999.
      do i = 1,ns
        jdx(i) = 0
      enddo
      nsktm = 0
      nsum = 0
      ntried = 0
      m = 0
      mm = 1
      mag = blank
      avxm = blank
      avfm = blank
c check value of eoff prior to calling phagt
21    if(eoff) goto 79
      call phagt(phcard, keyph, lph, inpt, injump, eoff, icent2, 
     *  itest)
cd    print *, 'phagt read ', lph, ' phase records'
      if(eoff) then
        if(inpt .eq. inmain) then
c         reading from batch input, so end of input job is near
          if(lph .le. 1) then
c this is the end
            write(punt, '(4a)') 
            write(logfil, *) 
     *      ' completed reading input phase file'
            if(punt .ne. logfil) then
              write(punt, '(4a)') 
     *        ' sound bell ', char(7), char(7), char(7)
              write(punt, *) 
     *        ' completed reading input phase file'
            endif
            goto 79
          else
c process one more event
            write(logfil, *) 'process one more event'
            if(punt .ne. logfil)
     *      write(punt, *) 'process one more event'
            goto 22
          endif
        else if(inpt .eq. injump) then
c         reading from jump file, so try main batch stream again
          eoff = .false.
          inpt = inmain
          write(logfil, 215)
          if(punt .ne. logfil)
     *    write(punt, 215)
215       format(' jump back to main input stream.')
          if(lph .eq. 1) goto 192
c         process the last jump event
          goto 22
        endif
      endif
22    continue
c
c first:  (mm = 1) - define time based on the 1st phase record
c second: (mm = 2) - try the first rejected time
25    do 27 l = 1, lph
      if( ( (keyph(l) .ge. 0)  .and. (mm .eq. 1) )  .or.
     *    ( (keyph(l) .eq. -4) .and. (mm .eq. 2) ) )then
        msta4 = phcard(l)(1:4)
        msta4 = dnstrg(rshft(msta4))
	comp = dnstrg(phcard(l)(6:6))

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!
	if(test(53) .eq. 1.0) then
	  if(msta4(1:1) .ne. ' ') then
	    if((msta4(4:4) .eq. 'n') .or. (msta4(4:4) .eq. 'e')) then
              comp = msta4(4:4)
	    endif
	  endif
	endif

	if((comp .eq. 'e') .or. (comp .eq. 'n')) then
	  msta(1) = msta4//comp
	else
	  msta(1) = msta4//'z'
	endif
c       skip station if not on station list
c       ho modificato qui: ho tolto phaind e messo ceck_staz
        call ceck_staz( msta(1), nsta, ns, i, ierr_ceck)

        if(ierr_ceck.ne.0) go to 27

	if((scatnow) .and. (scatph(i) .ne. ' ')) then
	  phcard(l)(18:24) = scatph(i)(18:24)
	  phcard(l)(32:36) = scatph(i)(32:36)
	endif

        read(phcard(l), 26, err = 265) jtime, jmin1, p1
26      format(bz, 9x, i8, i2, f5.2)
c define base time in minutes
        lbastm = jmin1
        ktime = jtime
        kdate = ktime/100
        ihr = ktime - kdate*100
	kday = kdate - (kdate/100)*100
c       kbstm = ihr*3600 + jmin1*60 + p1
        kbstm = 
     *    (kdate - (kdate/100)*100)*24*3600 + ihr*3600 + jmin1*60 + p1
        khrmn = 100*ihr + jmin1
        goto 28
c decode error
265     keyph(l) = -11
      endif
27    continue
28    m = 0
c
c begin main loop, dealing with each phase record ------------------
      do 78 l = 1, lph
cd    print *, ' '
cd    print *, 'main phasin loop, l = ', l
cd    print *, phcard(l)
c     phcard(l)(105:109) = dnstrg( phcard(l)(105:109) )
      msym(l) = ' '
c key for goto:  keyph   type of record
c                       0 --- phase record
c                      -1 --- comment record
c                      -2 --- summary record
c                      -3 --- instruction record
c                             (begins with more, dist, or 4 blanks)
c                      -4 --- bad time
c                      -5 --- reset record
c                      -6 --- save record - no longer used
c                      -7 --- rerun record - no longer used
c                      -8 --- scatter record
c                      -9 --- not on station list
c                     -10 --- jump record
c                     -11 --- decode error
c                     -12 --- sleep record
c                     -13 --- stop record
c                     -14 --- nongaus record
c		      -15 --- master scatter record
c
      if(keyph(l) .ge. 0) goto 29
c           comm  sum   inst  pha   reset save  rerun scatter
c           not-on jump  decode-er sleep stop nongaus master
      goto (78,   54,   56,   29,   62,   66,   66,   73,  
     *      29,    71,   76,       772,  774, 752,    757),
     *        abs(keyph(l))
c
c phase record
29      m = m + 1
        ldx(m) = 0
        keyph(l) = m
        keyphi(m) = l
        ksmp(m) = 1
cd      write(punt, '(a, i5)') 'beginning to process phase record # ', m
c if there is a decimal in 59-62, then this is an old c10 value
c so delete
        do 291 i = 59, 62
          if(phcard(l)(i:i) .eq. '.') then
            phcard(l)(59:62) = '    '
            goto 292
          endif
291     continue
292     msta4 = phcard(l)(1:4)
        msta4 = dnstrg(rshft(msta4))
	comp = dnstrg(phcard(l)(6:6))

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!

	if(test(53) .eq. 1.0) then
	  if(msta4(1:1) .ne. ' ') then
	    if((msta4(4:4) .eq. 'n') .or. (msta4(4:4) .eq. 'e')) then
              comp = msta4(4:4)
	    endif
	  endif
	endif

	if((comp .eq. 'n') .or. (comp .eq. 'e')) then
	  msta(m) = msta4//comp
	else
	  msta(m) = msta4//'z'
	endif
c find station in station list
c       ho modificato qui: ho tolto phaind e messo ceck_staz
        call ceck_staz( msta(m), nsta, ns, i, ierr_ceck2)
        if(ierr_ceck2 .ne. 0) then
          iskip = iskip + 1
          if(iskip .gt. 200) then
            write(logfil,83)
            if(punt .ne. logfil) write(punt,83)
            stop 'abort from phasin'
          endif
          write(16, 34) msta(m), phcard(l)
34        format(' ***>',a5, ' is not on station list, so next record ',
     *    ' will not be used:', /, a)
          keyph(l) = -9
          m = m - 1
          goto 78
        endif
c       kdx(phase number) = station number
        kdx(m) = i
	if(setmast) scatph(i) = phcard(l)
        if(setmast) write(punt, '(a, i5, 2a)') 
     *  ' station ', i, ' phase set to ', scatph(i)

c
c if this is a scatter run, where all events will have the same
c fixed arrival times but different added errors, change the p and s
c arrival times to the fixed values now.
	if((scatnow) .and. (scatph(i) .ne. ' ')) then
c         assume at this time that the yr mo dy hr of each phase is the same!
	  if(phcard(l)(18:24) .ne. ' ')
     *	   phcard(l)(18:24) = scatph(i)(18:24)
	  if(phcard(l)(32:36) .ne. ' ')
     *	   phcard(l)(32:36) = scatph(i)(32:36)
cd	  write(punt, '(a)') ' reset phase to: ', phcard(l)(1:36)
	endif
	

        read(phcard(l), 30, err = 76 )
     *  krmp(m),jtime,jmin(m),p(m),s(m),
     *  krms(m),amx(m),prx(m),krm(m),dt(m),fmp(m),
     *  poldev, pdev
30      format(bz, 4x, a4, 1x, i8, i2, f5.2, 7x, f5.2, a4, 3x, f4.0,
     *  f3.2, 12x, a2, 1x, f5.2, f5.0, 29x, 2a1)
        if(poldev .eq. ' ') poldev = bksrc
        poldev = dnstrg(poldev)
        if(pdev .eq. ' ') pdev = bksrc
        pdev = dnstrg(pdev)
        if(fmp(m) .le. 0.) fmp(m) = blank
        read(phcard(l), 31, err = 76)
     *        msym(m),ipwc,nlay(m),    iswc
31      format(6x, a1,  i1,     i1, 30x, i1)
cd      print *, 'original ipwc = ', ipwc
c convert msym to lower case
        msym(m) = dnstrg(msym(m))
c fix up amplitude.  if negative, then multiply by -10000
        if(amx(m) .lt. 0.) amx(m) = -amx(m)*10000.
c change first motion symbol to blank for horizontals
c (convention is to use n for north-south component, and e for e-w)
c also, remove magnitude information from horizontals
        if((dnstrg(phcard(l)(6:6)) .eq. 'n') .or.
     *  (dnstrg(phcard(l)(6:6)) .eq. 'e')) then
          fmp(m) = blank
          msym(m) = ' '
          amx(m) = 0.0
        endif

        if((scatp .ne. 0.0) .and. (.not. setmast)) then
          if(phcard(l)(20:24) .ne. '     ') then
c decide if this is the 1 in 10 with 10x greater error!
            fctor = 1.0
            if(ipwc .lt. 4) fctor =  rsew(ipwc+1)
            tail = ran3(0)
            if(tail .le. fract) then
              fctor = fctor*amplif
            endif 
            p(m) = p(m) + rnd()*scatp*fctor*fctmagp
            call riorbk(p(m), ipm, fmit, 5, 2)
            write(phcard(l)(20:24), fmit) ipm
          endif
        endif
        if((scats .ne. 0.0) .and. (.not. setmast)) then
          if(phcard(l)(32:36) .ne. '     ') then
            fctor = 1.0
            if(iswc .lt. 4) fctor =  rsew(iswc+1)
            tail = ran3(0)
            if(tail .le. fract) then
              fctor = fctor*amplif
            endif 
            s(m) = s(m) + rnd()*scats*fctor*fctmags
            call riorbk(s(m), ism, fmit, 5, 2)
            write(phcard(l)(32:36), fmit) ism
          endif
        endif


c compare time of current station to first station's time
c first compute equivalent of kbstm to see what offset is in seconds
	jhr = jtime - (jtime/100)*100
	jdate = jtime/100
	jday = jdate - (jdate/100)*100
	jbstm = 
     *  (jdate - (jdate/100)*100)*24*3600 + jhr*3600 + jmin(m)*60 + p(m)
	if((iabs(jbstm - kbstm) .gt. 600) .or. 
     *    (kdate/100 .ne. jdate/100)) then
          write(16, 37) phcard(l)
37        format(' ***> the next phase record has a deviant time',
     *    ', so it will not be used:', /, a)
          keyph(l) = -4
          m = m - 1
          nsktm = nsktm + 1
          if(nsktm .gt. lph/2) then
            if(ntried .eq. 1) goto 78
            seq = '     '  
            magke = ' '
            norem = 0
            iskip = 0
            pmin = 9999.
            do 38 iii = 1, ns
              jdx(iii) = 0
38          continue
            nsum = 0
            mm = 2
            write(16, 39) nsktm, lph
39          format(' ***> skipped', i5, ' out of', i5,' records due to',/
     *      ' deviant times, so will try a new base time once')
            nsktm = 0
            ntried = 1
            goto 25
          endif
          goto 78
        endif
c
c adjust jmin(m) for possible difference in day or hour
	jmin(m) = jmin(m) + (jday - kday)*24*60 + (jhr - ihr)*60

c       make sure station record has not expired.
40      ihrmn = 100*jhr + jmin(m)
        call diftim(icent2,jdate,ihrmn,ndate(i),nhr(i),iporm,difhr)
        if(difhr .gt. 0) then
c station has expired
c
cd        write(punt, '(a, 2i10)') ' expired on ', ndate(i), nhr(i)
      call update(indexs, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr, ns,
     *         exdly, icent2, kdate, ihrmn,
     *         infil, iofil)
cd          write(punt,333) indexs,nsta(i),lat(i),lon(i),kdate,ihrmn,
cd   1     sw(i),klas(1, i),calr(1, i),xmgc(i),xmwt(i),fmwt(i),
cd   2     fmgc(i),ipcod(i),iscod(i),tpdly(1,i),(exdly(kk,i),kk=1,4),
cd   2     tpdly(2,i),(revp(kk,i), kk = 1, 6), ndate(i),
cd   3     nhr(i),ielv(i),mod(i),ipthk(i),vthk(1,i),vthk(2,i),
cd   4     ipdly(i),dly(1,i),dly(2,i),dly(3,i),dly(4,i),dly(5,i),
cd   5     sdly(1,i),sdly(2,i),sdly(3,i),sdly(4,i),sdly(5,i),
cd   6     c(i),e(i),prr(i)
cd333      format(       1x,i3,     a4,        2f10.2,       2i10,/
cd   1     1x,f5.2,        i5,                   2f10.2, 2i5/
cd   2     1x,f10.2,            2i10,      f6.2,                  4a1,
cd   2         f6.2, /, 1x,               6a1,      i10,/
cd   3                             1x,4i10,              2f10.2,/
cd   4       1x,i10,                                     5f10.2,/
cd   5     1x,                                        5f10.2,/
cd   6     1x, 3f10.2/)
        endif
c*******
c for purposes of telemetry delay, some sources are equivalent
c (the same reduction is done on reading the station list)
c reduce all sources equivalent to usgs develocorder to 'v'
        if(pdev .eq. '*' .or. pdev .eq. '1' .or. pdev .eq. '4')
     *  pdev = 'v'
c reduce all sources equivalent to uagi develocorder to 'f'
        if ( pdev .eq. '%' .or. pdev .eq. 'a' ) pdev = 'f'
c reduce all sources equivalent to usgs mag tape to 's'
        if (pdev .eq. 'e' .or. pdev .eq. '2') pdev = 's'
c reduce all sources equivalent to uagi daq/dan or pc's to 'd'
c       if (pdev .eq. 'j' .or. pdev .eq. 'x'
c do not include 'p' because yakutat pc is 'p' and has no satellite delay. jcl 6/3/94
cc   *  .or. pdev .eq. 'p'
c    *  .or. pdev .eq. 'o' .or. pdev .eq. 'g'
c    *  .or. pdev .eq. 'k' .or. pdev .eq. 'i'
c    *  .or. pdev .eq. 'u') pdev = 'd'
c reduce all sources equivalent to uagi daq/dan 'd'
        if (pdev .eq. 'j' .or. pdev .eq. 'x') pdev = 'd'
c reduce all sources equivalent to uagi pc's to 'g'
        if (pdev .eq. 'o' 
     *  .or. pdev .eq. 'k' .or. pdev .eq. 'i'
     *  .or. pdev .eq. 'u') pdev = 'g'
c nhop related code has been removed, now that the station history
c allows more than one telemetry delay (tpdly) per station
c must fix up history to allow for no delay for yakutat pcelog
c         if(phcard(l)(110:110) .ne. ' ') then
c         read(phcard(l)(110:110), '(i1)') nhop
c         tpuse = -nhop*.27
c check the source of the p-phase (pdev) to see if tpdly 1 or 2 should be used
c use second delay if source matches any one of the 4 extra codes
c otherwise use the first delay
        if (pdev .eq. exdly(1, i) .or. pdev .eq. exdly(2, i) .or.
     *      pdev .eq. exdly(3, i) .or. pdev .eq. exdly(4, i)) then
          itpused(m) = 2
          tpuse = tpdly(itpused(m), i)
        else if ((pdev .eq. 't') .or. (pdev .eq. 'n')) then
c         for atwc teletype source "t", set telemetry delay to zero
c         for neis source "n", set telemetry delay to zero
          itpused(m) = 0
          tpuse = 0.0
        else
          itpused(m) = 1
          tpuse = tpdly(itpused(m), i)
        endif
        dt(m) = dt(m) + tpuse
cd        write(punt, '(a)') 'exdly, pdev, itpused(m), tpdly(1,i), tpdly(2,i)'
cd        write(punt, '(4a, 1x, a, i2, 2f6.2)') (exdly(kk, i), kk=1,4),
cd   *    pdev, itpused(m), tpdly(1,i), tpdly(2,i)
c******************************************************************
c take account of polarity reversals
c allow lower case, etc
        if(msym(m) .eq. 'u') msym(m) = 'c'
        if(msym(m) .eq. '.') msym(m) = ' '
        if(msym(m) .eq. 'c' .or. msym(m) .eq. 'd' .or.
     *     msym(m) .eq. '+' .or. msym(m) .eq. '-') then
c reduce all sources equivalent to usgs develocorder to 'v'
          if(poldev .eq. '*' .or. poldev .eq. '1' .or. poldev .eq. '4')
     *      poldev = 'v'
c reduce all sources equivalent to uagi develocorder to 'f'
          if ( poldev .eq. '%' .or. poldev .eq. 'a')
     *      poldev = 'f'
c reduce all sources equivalent to usgs mag tape to 's'
          if (poldev .eq. 'e' .or. poldev .eq. '2'
     *      .or. poldev .eq. 'p' .or. poldev .eq. 'o'
     *      .or. poldev .eq. 'g' .or. poldev .eq. 'k'
     *      .or. poldev .eq. 'i' .or. poldev .eq. 'u')
     *      poldev = 's'
c reduce all sources equivalent to uagi daq/dan
          if (poldev .eq. 'j' .or. poldev .eq. 'x')
     *      poldev = 'd'
          if (poldev .eq. 'v') then
            revers = revp(1, i)
          else if (poldev .eq. 's') then
            revers = revp(2, i)
          else if (poldev .eq. 'w') then
            revers = revp(3, i)
          else if (poldev .eq. 'f') then
            revers = revp(4, i)
          else if (poldev .eq. 'd') then
            revers = revp(5, i)
          else
c any other polarity device (could include ' ' if blank source = ' ')
            revers = revp(6, i)
          endif
c fix first motion symbols if reversed or uncertain
          if      ( revers .eq. 'n' .or. revers .eq. '+') then
c normal or probably normal polarity, so do nothing
          else if ( revers .eq. 'r' .or. revers .eq. '-') then
c reversed or probably reversed polarity, so reverse reading
            do 44 ifm = 1, 4
              if(msym(m) .eq. ifmsm(ifm)) then
                msym(m) = ifmsm(ifm + 4)
                goto 46
              endif
44          continue
46          continue
          else if ( revers .eq. ' ') then
c assume, for now, that blank polarity reversal indicator = probably normal
          else
c unknown is the only possibility left, so do not use reading
            msym(m) = '?'
          endif
        else if (msym(m) .eq. ' ' .or. msym(m) .eq. 'z' .or.
     *           msym(m) .eq. 'n') then
c blank, nodal, or noisy, so do nothing
        else
c any other code, set equal to ?
          write(16, 3) nsta(i), msym(m)
    3     format(/' station ', a4, ' first motion symbol (', a1,
     *    ') is not correct.  Reset to "?"')
          msym(m) = '?'
        endif
c put the corrected first motion into column 65 of the phase record
        phcard(l)(65:65) = msym(m)
        if((msym(m) .eq. 'c') .or. (msym(m) .eq. 'd') .or. 
     *     (msym(m) .eq. '+') .or.
     *     (msym(m) .eq. '-')) nfirst = nfirst + 1
c ldx = 0 if there is no s arrival or if the s arrival is
c used in an s-p interval.
        ldx(m) = 0
        jdx(i) = 1
        if(ndate(i) .eq. 99999998) then
c error, station has expired
          write(16, 466) nsta(i), kdate, ihrmn
466       format(' ***> xxxerrorxxx ', a4, ' has expired ',
     *    'and can not be used on ',i6.6,1x,i4)
          keyph(l) = -4
          m = m - 1
          goto 78
        endif
467     tp(m) = 60.*(jmin(m)-lbastm)+p(m)+dt(m)
        kwr(m) = '    '
        iqdo(m) = '    '
c
c fix up weights
c force weight code if ipcod or iscod on station record is not equal 10
c but set ww and wws equal to weights that would have been used
c do not use reading if field is blank
        if(phcard(l)(20:24) .eq. '     ') then
          ipwc = 8
cd       print *, 'ipwc changed to ', ipwc
          phcard(l)(8:8) = '4'
          krmp(m)(4:4) = '4'
        endif
        if(phcard(l)(32:36) .eq. '     ') then
          iswc = 4
        else
c set up s arrival pointer and time
          ldx(m) = 1
          ts(m) = 60.*(jmin(m)-lbastm)+s(m)+dt(m)
        endif
        ww(m) = 0.0
        wws(m) = 0.0
cd     print *, 'station weight sw(i) = ', sw(i)
        if(sw(i) .eq. 0.) then
c in this case, save original weights and ignore ipcod & iscod
c ignore for now case of s-p interval
          if(ipwc .lt. 4) ww(m) = rsew(ipwc+1)**(-2)
          w(m) = 0.
          ipwc = 5
          if(iswc .lt. 4) wws(m) = rsew(iswc+1)**(-2)
          ws(m) = 0.
          iswc = 5
cd       print *, 'w(m), ws(m) = ', w(m), ws(m)
          goto 78
        endif
c
cd     print *, 'p and s replacement codes = ', ipcod(i), iscod(i)
cd     print *, 'ipwc, iswc = ', ipwc, iswc
        if((ipwc .lt. 4) .or. (ipwc .eq. 9)) then
c if the reading was to be used, then check ipcod
          if(ipcod(i) .eq. 10) then
          else if((ipcod(i) .gt. 3) .and. (ipcod(i) .lt. 9)) then
c keep track of original weight for lissum (unless it was 9)
            if(ipwc .ne. 9) ww(m) = rsew(ipwc+1)**(-2)
            ipwc = ipcod(i)
          else if(ipcod(i) .eq. 9) then
            ipwc = 9
          else if(ipcod(i) .lt. 4) then
            ipwc = ipcod(i)
          endif
        endif
        if(iswc .lt. 4) then
c if the reading was to be used, then check iscod
          if(iscod(i) .eq. 10) then
          else if(iscod(i) .gt. 3) then
            wws(m) = test(39) * (rsew(iswc+1)**(-2))
            iswc = iscod(i)
          else if(iscod(i) .lt. 4) then
            iswc = iscod(i)
          endif
        endif
c
        if(ipwc .le. 3) then
c normal p arrival
          w(m) = rsew(ipwc+1)**(-2)
cd	  print *, 'normal p arrival ipwc, w(m) = ', ipwc, w(m)
c find earliest arrival time with w(m) .ne. 0.
          if(tp(m) .lt. pmin) then
            pmin = tp(m)
            near = m
          endif
        else if(ipwc .eq. 9) then
c
c s-p interval
cd       print *, 's-p interval'
          ksmp(m) = 0
c reset ldx, because s is being used in s-p interval
          ldx(m) = 0
          ts(m) = 60.*(jmin(m)-lbastm)+s(m)+dt(m)
c in this case use s weight code for setting weight of s-p interval
c normal s-p case in which there is an s phase arrival
          ws(m) = 0.0
          if(iswc .ge. 4) then
            w(m) = 0.0
          else
            w(m) = test(39) * (rsew(iswc+1)**(-2))
          endif
c do not use negative or zero s-p interval
          if(ts(m) .le. tp(m)) then
            w(m) = 0.0
          endif
          if( (tp(m) .lt. pmin) .and. (w(m) .gt. 0.0) ) then
            pmin = tp(m)
          endif
cd       print *, 's-p interval weight = w(m) = ', w(m)
          goto 78
        else
c
c p arrival with weight code of 4,5,6,7, or 8.
          w(m) = 0.
cd       print *, 'code of 4-8 gives w(m) = ', w(m)
        endif
c
c s arrival (except for s-p case that never reaches here)
        if(iswc .lt. 4) then
          ws(m) = test(39) * (rsew(iswc+1)**(-2))
        else
          ws(m) = 0.
        endif
        goto 78
c
c summary record
54      continue
        nsum = nsum + 1
        if(nsum .eq. 1) then
          seq = phcard(l)(92:96)
          evstat = phcard(l)(72:72)
          evtype = phcard(l)(90:90)
c only read date and hrmn if none was found on the phase records
          if(kdate .eq. 0) then
 	    read(phcard(l), '(i6, i4, t35, f2.1)') kdate, khrmn, odmag
	  else
 	    read(phcard(l), '(t35, f2.1)') odmag
	  endif
          magke = dnstrg(phcard(1)(78:78))
        endif
c
c special code for redoubt study - vary errors with magnitude
	if (scatp .lt. 0.0) then
          fctmagp = 0.005 + 0.165*exp(-1.87*odmag)
          fctmags = fctmagp
	  write(punt, '(a, 2f10.3, a, f5.2)') 
     *    ' add errors to p & s with standard errors = ', 
     *    fctmagp, scats*fctmags, ' for mag = ', odmag
	endif
c end of special section
c
        if((nsum .gt. 1) .or. (igsum .eq. 0)) goto 78
	if(magke .eq. 'k') then
c this is a "fake" summary record, so ignore starting location
	  malor = ' '
	  malla = ' '
	  malaz = ' '
	  goto 78
	endif
        read(phcard(l), 55)
     *  org1,org2,ala1,isnla,ala2,alo1,isnlo,alo2,zres,zres1
55      format(bz,8x,f2.0,f4.2,f2.0,a1,f4.2,f3.0,a1,f4.2,f5.2,
     *    t111, f5.2)
        isnla = dnstrg(isnla)
        isnlo = dnstrg(isnlo)
        malor = phcard(l)(9:14)
        malla = phcard(l)(15:18)
        malaz = phcard(l)(31:34)
 	if(phcard(l)(111:115) .ne. ' ') then
	  malaz = phcard(l)(112:115)
	  zres = zres1
	endif
c	write(punt, '(a, i2, a)') 'phcard(', l, ') is a summary record'
c	write(punt, '(a)') phcard(l)
c	write(punt, '(4a)') 'malor, malla, malaz = ', 
c    *    malor, malla, malaz
        goto 78
c
c instruction record
56      read(phcard(l), 57)
     *  ipro,ichec,knst,inst,szres,sla1,jsnla,sla2,slo1,jsnlo,slo2,
     *  srg1,srg2
57      format(2a4,9x,2i1,f5.2,16x,f2.0,a1,f5.2,5x,f3.0,a1,f5.2,11x,
     *  f2.0,f5.2)
        ipro = dnstrg(ipro)
        ichec = dnstrg(ichec)
        jsnla = dnstrg(jsnla)
        jsnlo = dnstrg(jsnlo)
	if(test(38) .eq. 4) then
	  knst = 1
	  inst = 7
	endif
        if (nsum .eq. 0) then
          evstat = phcard(l)(9:9)
          evtype = phcard(l)(10:10)
	  icent2 = itest(55)
        endif
        mamaz = phcard(l)(21:24)
        mamla = phcard(l)(43:46)
        mamor = phcard(l)(74:79)
        seqins = phcard(l)(92:96)
58      if(seq .eq. '     ') seq = seqins
c if the instruction rec is blank and there is a summary record
c     and the summary record was not ignored then use the summary
c     record.  conversely, if the instruction record is not blank or
c     there was no summary record or the summary record was ignored,
c     then use the instruction record.
59      if((mamla .eq. '    ') .and. (nsum .ge. 1) .and.
     *  (igsum .ne. 0)) goto 60
        malla = mamla
        ala1 = sla1
        isnla = jsnla
        ala2 = sla2
        alo1 = slo1
        isnlo = jsnlo
        alo2 = slo2
60      if((mamor .eq. ' ') .and. (nsum .ge. 1) .and.
     *  (igsum .ne. 0)) goto 61
        malor = mamor
        org1 = srg1
        org2 = srg2
61      if((mamaz .eq. '    ') .and. (nsum .ge. 1) .and.
     *  (igsum .ne. 0)) goto 80
        zres = szres
        malaz = mamaz
        goto 80
c
c reset record
62      continue
        if(inpt .eq. injump) then
c error- can not reset from jump file
          write(logfil, 622)
          if(punt .ne. logfil)
     *    write(punt, 622)
622       format(' xxxerrorxxx can not use a reset record in ',
     *    'a jump file')
          stop 'abort from phasin'
        endif
        write(punt, '(1x, a)') phcard(l)
        ipro = dnstrg(phcard(l)(1:4))
        ichec = dnstrg(phcard(l)(5:8))
        nr = lph
        if(nr .eq. 1) goto 90
        write(logfil,65) phcard(l)
        if(punt .ne. logfil)
     *  write(punt,65) phcard(l)
65      format(' ***** logic error at statement 28 of phasin *****',
     *  /,1x,a, /, ' so stop')
        stop 'abort from phasin'
c
c save and rerun
66      write(logfil, 67)
        if(punt .ne. logfil)
     *  write(punt, 67)
67      format(' xxxerrorxxx save and rerun are no longer valid ',
     *  'options.')
        stop 'abort from phasin'
c
c jump record
71      if(l .ne. injump) then
          write(logfil, 712)
          if(punt .ne. logfil)
     *    write(punt, 712)
712       format(' xxxerrorxxx jump record out of place.')
          stop 'abort from phasin'
        endif
        goto 21
c
c scatter record
73      read(phcard(l), 74) scatp, scats
74      format(19x, f5.2, 7x, f5.2)
	if(scatp .ge. 0.) then
	  fctmagp = 1.0
	  fctmags = 1.0
          write(punt,75) scatp,scats
75        format(1x,'scatter: p and s standard errors = ', 2f10.3)
	else
	  scatp = -1.0
          write(punt,'(a, /, a, f10.3)') 
     *     ' set p std err = 0.005 + 0.165*exp(-1.87*odmag)',
     *     ' set s std err = (p std err) * ', scats
        endif
        goto 21
c
c nongaus record
752     read(phcard(l), 74) fract, amplif
754     format(19x, f5.2, 7x, f5.2)
        write(punt,75) amplif, fract
756     format(1x,'nongaus: multiply p and s standard errors by ', 
     *  f10.3, 1x, f10.3, ' fraction of the time.')
        goto 21
c
c master scatter record
757     setmast = .true.
	scatnow = .false.
	write(punt, '(a)') 
     *  ' use this event as a master for random scatter.'
	do 758 i = 1, ns
	  scatph(i) = ' '
758	continue
	goto 21
c
c decode error in phase record
76      write(16, 77) phcard(l)
77      format(' ***> decode error reading this phase record:',/,1x, a)
        if(keyph(l) .ne. -11) then
          keyph(l) = -11
          m = m - 1
          goto 78
        endif
c
c sleep record
772     write(punt, '(1x, a)') phcard(l)
        nr = lph
        ipro = dnstrg(phcard(l)(1:4))
        ichec = dnstrg(phcard(l)(5:8))
        if(nr .eq. 1) goto 90
        write(punt,'(a,/,a)')
     *     ' *** error in phase data input structure ***',
     *     'sleep must be the first record of a new event'
        write(logfil,'(a,/,a)')
     *     ' ** error in phase data input structure **',
     *     'sleep must be the first record of a new event'
        stop 'abort from phasin'
c
c stop record
774     write(punt, '(1x, a)') phcard(l)
        nr = lph
        ipro = dnstrg(phcard(l)(1:4))
        ichec = dnstrg(phcard(l)(5:8))
        if(nr .eq. 1) goto 90
        write(punt,'(a,/,a)')
     *     ' *** error in phase data input structure ***',
     *     'stop must be the first record of a new event'
        write(logfil,'(a,/,a)')
     *     ' ** error in phase data input structure **',
     *     'stop must be the first record of a new event'
        stop 'abort from phasin'
c
78    continue
c
c  end of main loop -------------------------------------------------
c     if m = 0, then terminate program
c
79    nr = m
      if(nr .ge. 1) goto 84
      goto 90
c
80    nr = m
      if((nr .eq. 0) .and. (lph .gt. 1)) goto 90
      if(nr .ge. 1) goto 84
      write(logfil,81) phcard(lph)
      if(punt .ne. logfil)
     *write(punt,81) phcard(lph)
81    format(' xxxx this record skipped - out of place xxxx ',/,a)
      iskip = iskip + 1
      if(iskip .le. 200) then
c write out this event so that the phase file is preserved, for
c whatever that is worth!
        if((ipun .eq. 2) .or. (ipun .eq. 3)) then
          do 815 l = 1, lph
            write(11, '(a)') phcard(l)
815       continue
        endif
        goto 21
      else
82      write(logfil,83)
        if(punt .ne. logfil)
     *  write(punt,83)
83      format(' more than 200 records skipped, so stop')
        stop 'abort from phasin'
      endif
c
84    nrp = nr
      if((itest(38) .eq. 1) .or. (itest(38) .eq. 2)) knst = 1
      if(itest(38) .eq. 3) knst = 0
c define starting savla and savlo
      if(malla .ne. '    ') then
c define starting location from summary record or instruction record
        la = ala1 + .00001
        lo = alo1 + .00001
        call fold2(savla,savlo,la,isnla,ala2,lo,isnlo,alo2)
      else if((abs(test(3))+abs(test(4))) .gt. 0.00001) then
c       define starting location from test(3) and test(4)
        savla = latr
        savlo = lonr
      else
c       define starting location from first 10 p arrival times
c       do this in main routine if savla=savlo=99999.
        savla = 99999.
      endif
c begin definition of starting depth
      savez = 99999.
      if(malaz .ne. '    ') then
        savez = zres + test(8)
      endif
c define starting origin time
      savor = 99999.
      freor = .true.
      if(malor .ne. ' ') then
        savor = 60.*(org1 - lbastm) + org2
        freor = .false.
      else if(inst .eq. 8) then
        inst = 0
      endif
      noswt = 0
      nopwt = 0
      nos = 0
      notim = 0
      otim = 0.0
      do 89 i = 1,nrp
      ji = kdx(i)
      w(i) = w(i)*sw(ji)
      if(w(i) .ne. 0.0) nopwt = nopwt + 1
      if(ldx(i) .ne. 0) then
c   add s data to end of p data matrix
        nos = nos+1
        nrs = nrp+nos
c       ldx(p phase number) = s phase number
        ldx(i) = nrs
        ldx(nrs) = 0
        nlay(nrs) = 0
        tp(nrs) = ts(i)
        ww(nrs) = wws(i)
        if(ws(i) .ne. 0.0) then
          if(ts(i) .lt. tp(i)) then
c           do not use p or s if s time is earlier than p time
	    write(16, '(2a)') 
     *       ' ***> do not use p or s because s is earlier than p for ',
     *       nsta(ji)
            wws(i) = 0.
            ws(i) = 0.
            ww(nrs) = 0.
            w(i) = 0.
            ww(i) = 0.
          endif
        endif
        w(nrs) = ws(i)*sw(ji)
        if(w(nrs) .ne. 0.) noswt = noswt + 1
        if((w(i) .ne. 0.) .and. (w(nrs) .ne. 0.)) then
c     calculate origin time from p and s time
          otim = otim + tp(i) - (tp(nrs) - tp(i))/(test(1) - 1.0)
          notim = notim + 1
        endif
        ksmp(nrs) = 1
        dt(nrs) = dt(i)
        jmin(nrs) = jmin(i)
        kdx(nrs) = kdx(i)
        kwr(nrs) = '    '
        iqdo(nrs) = '    '
        msta(nrs) = msta(i)
        p(nrs) = p(i)
        s(nrs) = s(i)
      endif
89    continue
      nr = nrp+nos
      if((notim .ne. 0) .and. (savor .eq. 99999.)) then
        savor = otim/notim
        if(test(38) .lt. 0.0) inst = 8
      endif
c convert inst=7 to inst=9 with origin free - this is
c the option to use for quary blasts with unknown origin time.
      if(inst .eq. 7) then
        inst = 9
        freor = .true.
      endif
90    write(16, '(a)') 'end'
      if(setmast) scatnow = .true.
      return
      end
c end phasin
