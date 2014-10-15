!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module for Monte Carlo sampling in a 1D double well potential              !
! Written by                                                                 !
!                            Ye Mei                                          !
!                          10/14/2014                                        !
!                   East China Normal University                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module MC_DoubleWell
  use precision_m
  implicit none
  private
  integer(kind=4) :: Nstep
  real(kind=fp_kind) :: Temperature
  character(len=80) :: trajfile 

  real(kind=fp_kind), allocatable :: x(:)
  real(kind=fp_kind), allocatable :: U_x(:)
  integer(kind=fp_kind), allocatable ::isucc(:)
  real(kind=fp_kind), allocatable :: random_xdelta(:)
  real(kind=fp_kind), allocatable :: random_exp(:)
  public :: DoubleWell_initialize, DoubleWell_finalize, MC_DoubleWell_start
contains

  function U_DoubleWell(xi) result(U)
  use precision_m
  implicit none
  real(kind=fp_kind), intent(in) :: xi
  real(kind=fp_kind) :: U
  U = xi**2 + 1.d0 - sqrt( 4*xi**2 + 0.04d0 )
  end function U_DoubleWell 

  subroutine DoubleWell_initialize(ntotstep, x0, T, traj)
  use precision_m
  implicit none
  integer(kind=4), intent(in) :: ntotstep
  real(kind=fp_kind), intent(in) :: x0
  real(kind=fp_kind), intent(in) :: T
  real(kind=fp_kind) :: random_numbers(2*ntotstep)
  character(len=80) :: traj
  Nstep = ntotstep
  allocate(x(0:Nstep))
  allocate(U_x(0:Nstep))
  allocate(isucc(Nstep))
  allocate(random_xdelta(Nstep))
  allocate(random_exp(Nstep))
  x(0) = x0
  U_x(0) = U_DoubleWell(x0)
  temperature = T
  trajfile = traj
  call random_my(2*ntotstep, random_numbers)
  random_xdelta = random_numbers(1:ntotstep) - 0.5d0
  random_exp = random_numbers(ntotstep+1:2*ntotstep)
  open(99,file=trajfile)
  end subroutine DoubleWell_initialize

  subroutine DoubleWell_finalize
  use precision_m
  implicit none
  deallocate(x)
  deallocate(U_x)
  deallocate(isucc)
  close(99)
  end subroutine DoubleWell_finalize

  subroutine MC_DoubleWell_start
  use precision_m
  implicit none
  integer(kind=4) :: istep
  real(kind=fp_kind) :: xcurrent, Ucurrent, random_move, random_probability, xnext, Unext
  integer(kind=4) :: isuccess
  do istep = 1, Nstep
    xcurrent = x(istep-1)
    Ucurrent = U_x(istep-1)
    random_move = random_xdelta(istep)
    random_probability = random_exp(istep)
    call MC_Move(xcurrent, Ucurrent, random_move, random_probability, xnext, Unext, isuccess)
    x(istep) = xnext
    U_x(istep) = Unext
    isucc(istep) = isuccess
    write(99,'(I8,1X,F10.3,1X,F10.3,1X,I4)')istep, x(istep), U_x(istep), isucc(istep)
  end do
  end subroutine MC_DoubleWell_start

  subroutine MC_Move(xcurrent, Ucurrent, random_move, random_probability, xnext, Unext, isuccess)
  use precision_m
  implicit none
  real(kind=fp_kind), intent(in) :: xcurrent
  real(kind=fp_kind), intent(in) :: Ucurrent
  real(kind=fp_kind), intent(in) :: random_move
  real(kind=fp_kind), intent(in) :: random_probability
  real(kind=fp_kind), intent(out) :: xnext
  real(kind=fp_kind), intent(out) :: Unext
  integer(kind=4), intent(out) :: isuccess
 
  real(kind=fp_kind) :: xtrial
  real(kind=fp_kind) :: Utrial

  xtrial = xcurrent + random_move
  Utrial = U_DoubleWell(xtrial)
  if ( Utrial <= Ucurrent )then
    isuccess = 1
    xnext = xtrial
    Unext = Utrial
  else 
    if( exp(-(Utrial-Ucurrent)/temperature) > random_probability ) then
      isuccess = 1
      xnext = xtrial
      Unext = Utrial
    else
      isuccess = 0
      xnext = xcurrent
      Unext = Ucurrent
    end if
  end if
  end subroutine MC_Move
end module MC_DoubleWell
