!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module for Monte Carlo sampling in a 1D double well potential              !
! Written by                                                                 !
!                                        Ye Mei                              !
!                                      10/14/2014                            !
!                           East China Normal University                     !
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module DoubleWell
  use precision_m
  implicit none
  private
  integer(kind=4), public :: Nsteps
  real(kind=fp_kind), public :: Temperature
 
  real(kind=fp_kind), allocatable :: x(:)
  real(kind=fp_kind), allocatable :: U_x(:)
  
  
contains

  function U_DoubleWell(xi) result(U)
  use precision_m
  implicit none
  real(kind=fp_kind), intent(in) :: xi
  real(kind=fp_kind) :: U
  U = xi**2 + 1.d0 + sqrt( 4*xi**2 + 0.04d0 )
  end function U_DoubleWell 

  subroutine DoubleWell_initialize(totstep,x0)
  use precision_m
  implicit none
  integer(kind=4), intent(in) ::totstep
  real(kind=fp_kind), intent(in) :: x0
  Nstep = totstep
  x = x0
  allocate(x(Nstep))
  allocate(U_x(Nstep))
  end subroutine DoubleWell_initialize

  subroutine DoubleWell_finalize()
  use precision_m
  implicit none
  deallocate(x)
  deallocate(U_x)
  end subroutine DoubleWell_finalize

  subroutine MC_Move
  use precision_m
  implicit none
  integer(kind=4) :: istep
  real(kind=fp_kind) :: xi, xtrial
  real(kind=fp_kind) :: U_x_i, U_x_trial
  real(kind=fp_kind) :: x_delta

  x_delta = random_my

  end subroutine MC_Move
end module DoubleWell
