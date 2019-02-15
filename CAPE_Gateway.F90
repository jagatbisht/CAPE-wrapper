!----------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------
! Programmer: Jagat BIsht
! Matlab wrapper to compupt CAPE/CIN
!-----------------------------------------------------------------------
!
!  getcape - MATLAB wrapper for a fortran90 subroutine to calculate Convective Available
!            Potential Energy (CAPE) from a sounding.
!
!  Disclaimer:  This code is made available WITHOUT WARRANTY.

!

#include "fintrf.h"

!\
! The following macros are needed for older versions of MATLAB that do not have these
! macros defined in the fintrf.h file.
!/

#ifndef mwSize
#define mwSize integer(4)
#endif

#ifndef mwPointer
#define mwPointer integer(4)
#endif

#ifndef mwIndex
#define mwIndex integer(4)
#endif

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module MatlabAPImex
     
!-----------------------------------------------------      
! Interface definitions for MATLAB API mex functions
!-----------------------------------------------------
     
      interface
!-----
      integer(4) function mexPrintf(message)
      character(len=*), intent(in) :: message
      end function mexPrintf
!-----
      end interface

      end module MatlabAPImex

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module MatlabAPImx
   
!-----------------------------------------------------      
! Interface definitions for MATLAB API mx functions
!-----------------------------------------------------
     
      interface
!-----
      mwPointer function mxDuplicateArray(pm)
      mwPointer, intent(in) :: pm
      end function mxDuplicateArray
!-----
      subroutine mxFree(ptr)
      mwPointer, intent(in) :: ptr
      end subroutine mxFree
!-----
      mwPointer function mxGetDimensions(pm)
      mwPointer, intent(in) :: pm
      end function mxGetDimensions
!-----
      mwSize function mxGetNumberOfDimensions( mx )
      mwPointer, intent(in) :: mx
      end function mxGetNumberOfDimensions
!-----
      mwPointer function mxGetPr( mx )
      mwPointer, intent(in) :: mx
      end function mxGetPr
!-----
      integer(4) function mxIsDouble( mx )
      mwPointer, intent(in) :: mx
      end function mxIsDouble
!-----
      integer(4) function mxIsSparse(pm)
      mwPointer, intent(in) :: pm
      end function mxIsSparse
!-----
      mwPointer function mxMalloc(n)
      mwSize, intent(in) :: n
      end function mxMalloc
!-----
      end interface

      end module MatlabAPImx

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module MatlabAPIfp
      use MatlabAPImx
     
!-----------------------------------------------------      
! Interface definitions for Fortan Pointer functions
!-----------------------------------------------------
     
      interface fpGetPr3
          module procedure fpGetPr3Double
      end interface

      interface fpGetPr2
          module procedure fpGetPr2Double
      end interface

      interface fpGetPr2I
          module procedure fpGetPr2Int
      end interface

      interface fpAllocate
          module procedure fpAllocate3Double
      end interface
     
      interface fpDeallocate
          module procedure fpDeallocate3Double
      end interface
     
!-----------------------------------------------------      

      contains
     
!-----------------------------------------------------      
! Specific Fortran Pointer functions
!-----------------------------------------------------
     
!----------------------------------------------------------------------
      function fpGetPr3Double( mx ) result(fp)
      implicit none
      real(8), pointer :: fp(:,:,:)
!-ARG
      mwPointer, intent(in) :: mx
!-COM
      real(8), pointer :: Apx3(:,:,:)
      common /MatlabAPI_COMA/ Apx3
!-LOC
      mwPointer :: pr
      mwSize :: ndim
      mwSize, pointer :: dims(:)
      mwSize :: dimz(3)
!-----
      ndim = mxGetNumberOfDimensions(mx)
      if( mxIsDouble(mx) == 1 .and. mxIsSparse(mx) == 0 .and.           &
     &    ndim <= 3 ) then
          pr = mxGetPr( mx )
          dims => fpGetDimensions( mx )
          dimz = 1
          dimz(1:ndim) = dims
          call MatlabAPI_COM_Apx3( %val(pr), dimz )
          fp => Apx3
      else
          nullify( fp )
      endif
      return
      end function fpGetPr3Double
!---------------------------------------------------------------------
      function fpGetPr2Double( mx ) result(fp)
      implicit none
      real(8), pointer :: fp(:,:)
!-ARG
      mwPointer, intent(in) :: mx
!-COM
      real(8), pointer :: Apx2(:,:)
      common /MatlabAPI_COMA2/ Apx2
!-LOC
      mwPointer :: pr
      mwSize :: ndim
      mwSize, pointer :: dims(:)
      mwSize :: dimz(2)
!-----

     if( mxIsDouble(mx) == 1 .and. mxIsSparse(mx) == 0 .and.           &
     &    ndim <= 2 ) then
          pr = mxGetPr( mx )
          dims => fpGetDimensions( mx )
          call MatlabAPI_COM_Apx2( %VAL(pr), dimz)
          fp => Apx2
      else
          nullify( fp )
      endif  
      return
      end function fpGetPr2Double
!---------------------------------------------------------------------
      function fpGetPr2Int( mx ) result(fp)
      implicit none
      integer*8 , pointer :: fp(:,:)
!-ARG
      mwPointer, intent(in) :: mx
!-COM
      integer*8, pointer :: Apx2I(:,:)
!      common /MatlabAPI_COMA2/ Apx2I
!-LOC
      mwPointer :: pr
      mwSize :: ndim
      mwSize, pointer :: dims(:)
      mwSize :: dimz(2)
!-----

!     if( mxIsDouble(mx) == 1 .and. mxIsSparse(mx) == 0 .and.           &
!     &    ndim <= 2 ) then
          pr = mxGetPr( mx )
          dims => fpGetDimensions( mx )
          call MatlabAPI_COM_Apx2I( %VAL(pr), dimz)
          fp => Apx2I
!      else
!          nullify( fp )
!      endif  
      return
      end function fpGetPr2Int
!---------------------------------------------------------------------
      function fpGetDimensions( mx ) result(fp)
      implicit none
      mwSize, pointer :: fp(:)
!-ARG
      mwPointer, intent(in) :: mx
!-COM
      mwSize, pointer :: Dpx(:)
      common /MatlabAPI_COMD/ Dpx
!-LOC
      mwSize :: ndim
      mwPointer :: dims
!-----
      ndim = mxGetNumberOfDimensions( mx )
      dims = mxGetDimensions( mx )
      call MatlabAPI_COM_Dpx(%val(dims), ndim)
      fp => Dpx
      end function fpGetDimensions
!----------------------------------------------------------------------
      function fpAllocate3Double( n1, n2, n3 ) result(fp)
      implicit none
      real(8), pointer :: fp(:,:,:)
!-ARG
      mwSize, intent(in) :: n1, n2, n3
!-COM
      real(8), pointer :: Apx3(:,:,:)
      common /MatlabAPI_COMA/ Apx3
!-LOC
      mwPointer :: mxmemory
!-----
      mxmemory = mxMalloc(n1*n2*n3*8)
      call MatlabAPI_COM_Apx3( %val(mxmemory), (/n1,n2,n3/) )
      fp => Apx3
      return
      end function fpAllocate3Double
!----------------------------------------------------------------------
      subroutine fpDeallocate3Double( fp )
      implicit none
!-ARG
      real(8), pointer, intent(inout) :: fp(:,:,:)
!-LOC
      mwPointer :: mxmemory
!-----
      if( associated(fp) ) then
          mxmemory = loc(fp)
          call mxFree(mxmemory)
          nullify(fp)
      endif
      return
      end subroutine fpDeallocate3Double

      end module MatlabAPIfp

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
     
!----------------------------------------------------------------------
! Specific Fortan Helper functions. Not contained in the module because
! we need an implicit interface to get the %val() construct to work
! properly in the calling routine. Passing the appropriate pointer back
! in a COMMON block. Looks awkward, but works beautifully.
!----------------------------------------------------------------------
      subroutine MatlabAPI_COM_Apx3( A, DIMS )
      implicit none
!-ARG
      mwSize, intent(in) :: DIMS(3)
      real(8), target, intent(in) :: A(DIMS(1),DIMS(2),DIMS(3))
!-COM
      real(8), pointer :: Apx3(:,:,:)
      common /MatlabAPI_COMA/ Apx3
!-----
      Apx3 => A
      return
      end subroutine MatlabAPI_COM_Apx3
!----------------------------------------------------------------------
      subroutine MatlabAPI_COM_Apx2( A, DIMS )
      implicit none
!-ARG
      mwSize, intent(in) :: DIMS(2)
      real(8), target, intent(in) :: A(DIMS(1),DIMS(2))
!-COM
      real(8), pointer :: Apx2(:,:)
      common /MatlabAPI_COMA2/ Apx2
!-----
      Apx2 => A
      return
      end subroutine MatlabAPI_COM_Apx2
!----------------------------------------------------------------------
      subroutine MatlabAPI_COM_Apx2I( A, DIMS )
      implicit none
!-ARG
      mwSize, intent(in) :: DIMS(2)
      integer*8, target, intent(in) :: A(DIMS(1),DIMS(2))
!-COM
      integer*8, pointer :: Apx2I(:,:)
!      common /MatlabAPI_COMA2/ Apx2I
!-----
      Apx2I => A
      return
      end subroutine MatlabAPI_COM_Apx2I
!----------------------------------------------------------------------      
      subroutine MatlabAPI_COM_Dpx(dims, ndim)
      implicit none
!-ARG
      mwSize, intent(in) :: ndim
      mwSize, target, intent(in) :: dims(ndim)
!-COM
      mwSize, pointer :: Dpx(:)
      common /MatlabAPI_COMD/ Dpx
!-----
      Dpx => dims
      return
      end subroutine MatlabAPI_COM_Dpx

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------

     subroutine mexFunction(nlhs, plhs, nrhs, prhs)
      use MatlabAPImex
      use MatlabAPImx
      use MatlabAPIfp
      implicit none
!-ARG
      mwPointer plhs(*), prhs(*)
      integer*4 nlhs, nrhs
      real*8 :: mxGetScalar
      mwPointer mxCreateNumericArray
      mwPointer mxCreateDoubleMatrix
      mwPointer mxCreateNumericMatrix
      mwPointer x_pr1,x_pr2,x_pr3, y_pr

!-LOC
      integer*4 :: nk
      real(8), pointer :: p_in(:,:)
      real(8), pointer :: t_in(:,:)
      real(8), pointer :: td_in(:,:)
      real(8), pointer :: cape(:,:)
      real(8), pointer :: cin(:,:)


      integer*4 numel
      mwSize, pointer :: dims(:)
      mwSize dimz(2)
      mwSize, parameter :: ndim = 2
      mwSize s

      character(len=100) line
      integer*4, parameter :: mxREAL = 0
      integer*4, parameter :: mxDOUBLE_CLASS   =  6
      integer*4, parameter :: mxINT8_CLASS = 8
      integer*4 mxClassIDFromClassName
      integer*4 classid
      integer*4 complexflag    
!-----
!\
! Check the input
!/
      if( nrhs /= 4 ) then
          call mexErrMsgTxt("Need one 2D double input")
      endif
      if( nlhs > 2 ) then
          call mexErrMsgTxt("Too many outputs")
      endif
!\
! Get a Fortran pointer to the input variable data area.
!/
      nk = mxGetScalar ( prhs(1) )

      p_in => fpGetPr2(prhs(2))
      if( .not.associated(p_in) ) then
          call mexErrMsgTxt("Input is not 1D double")
      endif
      t_in => fpGetPr2(prhs(3))
      if( .not.associated(t_in) ) then
          call mexErrMsgTxt("Input is not 1D double")
      endif
      td_in => fpGetPr2(prhs(4))
      if( .not.associated(td_in) ) then
          call mexErrMsgTxt("Input is not 1D double")
      endif 

!\
! Generate output variable and get pointer to data area
!/
      dims => fpGetDimensions(prhs(2))

      plhs(1) = mxCreateNumericMatrix(1, 1,                  &
      &   mxDOUBLE_CLASS,mxREAL)

      cape=> fpGetPr2(plhs(1))

      call  getcape(nk, p_in, t_in, td_in, cape, cin)
      
      plhs(2) = mxCreateNumericMatrix(1, 1,                  &
      &   mxDOUBLE_CLASS,mxREAL)

      cin=> fpGetPr2(plhs(2))


      return
      end
!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------

    subroutine getcape( nk , p_in , t_in , td_in, cape , cin )
    implicit none

    integer, intent(in) :: nk
    real, dimension(nk), intent(in) :: p_in,t_in,td_in
    real, intent(out) :: cape,cin


!-----------------------------------------------------------------------
!
!  Input:     nk - number of levels in the sounding (integer)
!
!           p_in - one-dimensional array of pressure (mb) (real)
!
!           t_in - one-dimensional array of temperature (C) (real)
!
!          td_in - one-dimensional array of dewpoint temperature (C) (real)
!
!  Output:  cape - Convective Available Potential Energy (J/kg) (real)
!
!            cin - Convective Inhibition (J/kg) (real)
!
!-----------------------------------------------------------------------
!  User options:

    real, parameter :: pinc = 100.0   ! Pressure increment (Pa)
                                      ! (smaller number yields more accurate
                                      !  results,larger number makes code 
                                      !  go faster)

    integer, parameter :: source = 2    ! Source parcel:
                                        ! 1 = surface
                                        ! 2 = most unstable (max theta-e)
                                        ! 3 = mixed-layer (specify ml_depth)

    real, parameter :: ml_depth =  200.0  ! depth (m) of mixed layer 
                                          ! for source=3

    integer, parameter :: adiabat = 1   ! Formulation of moist adiabat:
                                        ! 1 = pseudoadiabatic, liquid only
                                        ! 2 = reversible, liquid only
                                        ! 3 = pseudoadiabatic, with ice
                                        ! 4 = reversible, with ice

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------
!            No need to modify anything below here:
!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------

    logical :: doit,ice,cloud,not_converged
    integer :: k,kmax,n,nloop,i,orec
    real, dimension(nk) :: p,t,td,pi,q,th,thv,z,pt,pb,pc,pn,ptv

    real :: the,maxthe,parea,narea,lfc
    real :: th1,p1,t1,qv1,ql1,qi1,b1,pi1,thv1,qt,dp,dz,ps,frac
    real :: th2,p2,t2,qv2,ql2,qi2,b2,pi2,thv2
    real :: thlast,fliq,fice,tbar,qvbar,qlbar,qibar,lhv,lhs,lhf,rm,cpm
    real*8 :: avgth,avgqv
    real :: getqvs,getqvi,getthe

!-----------------------------------------------------------------------

    real, parameter :: g     = 9.81
    real, parameter :: p00   = 100000.0
    real, parameter :: cp    = 1005.7
    real, parameter :: rd    = 287.04
    real, parameter :: rv    = 461.5
    real, parameter :: xlv   = 2501000.0
    real, parameter :: xls   = 2836017.0
    real, parameter :: t0    = 273.15
    real, parameter :: cpv   = 1875.0
    real, parameter :: cpl   = 4190.0
    real, parameter :: cpi   = 2118.636
    real, parameter :: lv1   = xlv+(cpl-cpv)*t0
    real, parameter :: lv2   = cpl-cpv
    real, parameter :: ls1   = xls+(cpi-cpv)*t0
    real, parameter :: ls2   = cpi-cpv

    real, parameter :: rp00  = 1.0/p00
    real, parameter :: eps   = rd/rv
    real, parameter :: reps  = rv/rd
    real, parameter :: rddcp = rd/cp
    real, parameter :: cpdrd = cp/rd
    real, parameter :: cpdg  = cp/g

    real, parameter :: converge = 0.0002

    integer, parameter :: debug_level =   0

!-----------------------------------------------------------------------

!---- convert p,t,td to mks units; get pi,q,th,thv ----!

    do k=1,nk
        p(k) = 100.0*p_in(k)
        t(k) = 273.15+t_in(k)
       td(k) = 273.15+td_in(k)
       pi(k) = (p(k)*rp00)**rddcp
        q(k) = getqvs(p(k),td(k))
       th(k) = t(k)/pi(k)
      thv(k) = th(k)*(1.0+reps*q(k))/(1.0+q(k))
    enddo

!---- get height using the hydrostatic equation ----!

    z(1) = 0.0
    do k=2,nk
      dz = -cpdg*0.5*(thv(k)+thv(k-1))*(pi(k)-pi(k-1))
      z(k) = z(k-1) + dz
    enddo

!---- find source parcel ----!

  IF(source.eq.1)THEN
    ! use surface parcel
    kmax = 1

  ELSEIF(source.eq.2)THEN
    ! use most unstable parcel (max theta-e)

    IF(p(1).lt.50000.0)THEN
      ! first report is above 500 mb ... just use the first level reported
      kmax = 1
      maxthe = getthe(p(1),t(1),td(1),q(1))
    ELSE
      ! find max thetae below 500 mb
      maxthe = 0.0
      do k=1,nk
        if(p(k).ge.50000.0)then
          the = getthe(p(k),t(k),td(k),q(k))
          if( the.gt.maxthe )then
            maxthe = the
            kmax = k
          endif
        endif
      enddo
    ENDIF
    if(debug_level.ge.100) print *,'  kmax,maxthe = ',kmax,maxthe

  ELSEIF(source.eq.3)THEN
    ! use mixed layer

    IF( (z(2)-z(1)).gt.ml_depth )THEN
      ! the second level is above the mixed-layer depth:  just use the
      ! lowest level

      avgth = th(1)
      avgqv = q(1)
      kmax = 1

    ELSEIF( z(nk).lt.ml_depth )THEN
      ! the top-most level is within the mixed layer:  just use the
      ! upper-most level

      avgth = th(nk)
      avgqv = q(nk)
      kmax = nk

    ELSE
      ! calculate the mixed-layer properties:

      avgth = 0.0
      avgqv = 0.0
      k = 2
      if(debug_level.ge.100) print *,'  ml_depth = ',ml_depth
      if(debug_level.ge.100) print *,'  k,z,th,q:'
      if(debug_level.ge.100) print *,1,z(1),th(1),q(1)

      do while( (z(k).le.ml_depth) .and. (k.le.nk) )

        if(debug_level.ge.100) print *,k,z(k),th(k),q(k)

        avgth = avgth + 0.5*(z(k)-z(k-1))*(th(k)+th(k-1))
        avgqv = avgqv + 0.5*(z(k)-z(k-1))*(q(k)+q(k-1))

        k = k + 1

      enddo

      th2 = th(k-1)+(th(k)-th(k-1))*(ml_depth-z(k-1))/(z(k)-z(k-1))
      qv2 =  q(k-1)+( q(k)- q(k-1))*(ml_depth-z(k-1))/(z(k)-z(k-1))

      if(debug_level.ge.100) print *,999,ml_depth,th2,qv2

      avgth = avgth + 0.5*(ml_depth-z(k-1))*(th2+th(k-1))
      avgqv = avgqv + 0.5*(ml_depth-z(k-1))*(qv2+q(k-1))

      if(debug_level.ge.100) print *,k,z(k),th(k),q(k)

      avgth = avgth/ml_depth
      avgqv = avgqv/ml_depth

      kmax = 1

    ENDIF

    if(debug_level.ge.100) print *,avgth,avgqv

  ELSE

    print *
    print *,'  Unknown value for source'
    print *
    print *,'  source = ',source
    print *
    stop

  ENDIF

!---- define parcel properties at initial location ----!
    narea = 0.0

  if( (source.eq.1).or.(source.eq.2) )then
    k    = kmax
    th2  = th(kmax)
    pi2  = pi(kmax)
    p2   = p(kmax)
    t2   = t(kmax)
    thv2 = thv(kmax)
    qv2  = q(kmax)
    b2   = 0.0
  elseif( source.eq.3 )then
    k    = kmax
    th2  = avgth
    qv2  = avgqv
    thv2 = th2*(1.0+reps*qv2)/(1.0+qv2)
    pi2  = pi(kmax)
    p2   = p(kmax)
    t2   = th2*pi2
    b2   = g*( thv2-thv(kmax) )/thv(kmax)
  endif

    ql2 = 0.0
    qi2 = 0.0
    qt  = qv2

    cape = 0.0
!    cin  = 0.0
    lfc  = 0.0

    doit = .true.
    cloud = .false.
    if(adiabat.eq.1.or.adiabat.eq.2)then
      ice = .false.
    else
      ice = .true.
    endif

      the = getthe(p2,t2,t2,qv2)
      if(debug_level.ge.100) print *,'  the = ',the

!---- begin ascent of parcel ----!

      if(debug_level.ge.100)then
        print *,'  Start loop:'
        print *,'  p2,th2,qv2 = ',p2,th2,qv2
      endif

    do while( doit .and. (k.lt.nk) )

        k = k+1
       b1 =  b2

       dp = p(k-1)-p(k)

      if( dp.lt.pinc )then
        nloop = 1
      else
        nloop = 1 + int( dp/pinc )
        dp = dp/float(nloop)
      endif

      do n=1,nloop

         p1 =  p2
         t1 =  t2
        pi1 = pi2
        th1 = th2
        qv1 = qv2
        ql1 = ql2
        qi1 = qi2
        thv1 = thv2

        p2 = p2 - dp
        pi2 = (p2*rp00)**rddcp

        thlast = th1
        i = 0
        not_converged = .true.

        do while( not_converged )
          i = i + 1
          t2 = thlast*pi2
          if(ice)then
            fliq = max(min((t2-233.15)/(273.15-233.15),1.0),0.0)
            fice = 1.0-fliq
          else
            fliq = 1.0
            fice = 0.0
          endif
          qv2 = min( qt , fliq*getqvs(p2,t2) + fice*getqvi(p2,t2) )
          qi2 = max( fice*(qt-qv2) , 0.0 )
          ql2 = max( qt-qv2-qi2 , 0.0 )

          tbar  = 0.5*(t1+t2)
          qvbar = 0.5*(qv1+qv2)
          qlbar = 0.5*(ql1+ql2)
          qibar = 0.5*(qi1+qi2)

          lhv = lv1-lv2*tbar
          lhs = ls1-ls2*tbar
          lhf = lhs-lhv

          rm=rd+rv*qvbar
          cpm=cp+cpv*qvbar+cpl*qlbar+cpi*qibar
          th2=th1*exp(  lhv*(ql2-ql1)/(cpm*tbar)     &
                       +lhs*(qi2-qi1)/(cpm*tbar)     &
                       +(rm/cpm-rd/cp)*alog(p2/p1) )

          if(i.gt.90) print *,i,th2,thlast,th2-thlast
          if(i.gt.100)then
            print *
            print *,'  Error:  lack of convergence'
            print *
            print *,'  ... stopping iteration '
            print *
            stop 1001
          endif
          if( abs(th2-thlast).gt.converge )then
            thlast=thlast+0.3*(th2-thlast)
          else
            not_converged = .false.
          endif
        enddo

        ! Latest pressure increment is complete.  Calculate some
        ! important stuff:

        if( ql2.ge.1.0e-10 ) cloud = .true.

        IF(adiabat.eq.1.or.adiabat.eq.3)THEN
          ! pseudoadiabat
          qt  = qv2
          ql2 = 0.0
          qi2 = 0.0
        ELSEIF(adiabat.le.0.or.adiabat.ge.5)THEN
          print *
          print *,'  Undefined adiabat'
          print *
          stop 10000
        ENDIF

      enddo

      thv2 = th2*(1.0+reps*qv2)/(1.0+qv2+ql2+qi2)
        b2 = g*( thv2-thv(k) )/thv(k)
        dz = -cpdg*0.5*(thv(k)+thv(k-1))*(pi(k)-pi(k-1))

      the = getthe(p2,t2,t2,qv2)

      ! Get contributions to CAPE and CIN:

      if( (b2.ge.0.0) .and. (b1.lt.0.0) )then
        ! first trip into positive area
        ps = p(k-1)+(p(k)-p(k-1))*(0.0-b1)/(b2-b1)
        frac = b2/(b2-b1)
        parea =  0.5*b2*dz*frac
        narea = narea-0.5*b1*dz*(1.0-frac)
        if(debug_level.ge.200)then
          print *,'      b1,b2 = ',b1,b2
          print *,'      p1,ps,p2 = ',p(k-1),ps,p(k)
          print *,'      frac = ',frac
          print *,'      parea = ',parea
          print *,'      narea = ',narea
        endif
!        cin  = cin  + narea
        narea = 0.0
      elseif( (b2.lt.0.0) .and. (b1.gt.0.0) )then
        ! first trip into neg area
        ps = p(k-1)+(p(k)-p(k-1))*(0.0-b1)/(b2-b1)
        frac = b1/(b1-b2)
        parea =  0.5*b1*dz*frac
        narea = -0.5*b2*dz*(1.0-frac)
        if(debug_level.ge.200)then
          print *,'      b1,b2 = ',b1,b2
          print *,'      p1,ps,p2 = ',p(k-1),ps,p(k)
          print *,'      frac = ',frac
          print *,'      parea = ',parea
          print *,'      narea = ',narea
        endif
      elseif( b2.lt.0.0 )then
        ! still collecting negative buoyancy
        parea =  0.0
        narea = narea-0.5*dz*(b1+b2)
      else
        ! still collecting positive buoyancy
        parea =  0.5*dz*(b1+b2)
        narea =  0.0
      endif

      cape = cape + max(0.0,parea)

!      if(debug_level.ge.200)then
!        write(6,102) p2,b1,b2,cape,cin,cloud
!102     format(5(f13.4),2x,l1)
!      endif

      if( (p(k).le.10000.0).and.(b2.lt.0.0) )then
        ! stop if b < 0 and p < 100 mb
        doit = .false.
      endif

    enddo

!---- All done ----!

    return
    end subroutine getcape

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------

    real function getqvs(p,t)
    implicit none

    real :: p,t,es

    real, parameter :: eps = 287.04/461.5

    es = 611.2*exp(17.67*(t-273.15)/(t-29.65))
    getqvs = eps*es/(p-es)

    return
    end function getqvs

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------

    real function getqvi(p,t)
    implicit none

    real :: p,t,es

    real, parameter :: eps = 287.04/461.5

    es = 611.2*exp(21.8745584*(t-273.15)/(t-7.66))
    getqvi = eps*es/(p-es)

    return
    end function getqvi

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------

    real function getthe(p,t,td,q)
    implicit none

    real :: p,t,td,q
    real :: tlcl

    if( (td-t).ge.-0.1 )then
      tlcl = t
    else
      tlcl = 56.0 + ( (td-56.0)**(-1) + 0.00125*alog(t/td) )**(-1)
    endif

    getthe=t*( (100000.0/p)**(0.2854*(1.0-0.28*q)) )   &
            *exp( ((3376.0/tlcl)-2.54)*q*(1.0+0.81*q) )

    return
    end function getthe

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------
