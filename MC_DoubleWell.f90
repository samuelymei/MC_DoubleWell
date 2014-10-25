!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module for Monte Carlo sampling in a 1D double well potential              !
! Written by                                                                 !
!                            Ye Mei                                          !
!                          10/25/2014                                        !
!                   East China Normal University                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module MC_DoubleWell
  use precision_m
  implicit none
  private
  integer(kind=4) :: nStep
  character(len=80) :: trajfile 

  type, public :: state_info
    real(kind=fp_kind) :: x
    real(kind=fp_kind) :: u
    integer(kind=4) :: isucc
  end type state_info
  type(state_info), allocatable, public :: states(:)

  real(kind=fp_kind) :: Temperature
  real(kind=fp_kind), allocatable :: random_xdelta(:)
  real(kind=fp_kind), allocatable :: random_exp(:)
  public :: DoubleWell_initialize, DoubleWell_finalize, MC_DoubleWell_run
  public :: calcEnergy
contains

  subroutine calcEnergy(state) 
    use precision_m
    implicit none
    type (state_info), intent(in out) :: state
    state%u = state%x**2 + 1.d0 - sqrt( 4*state%x**2 + 0.04d0 )
  end subroutine calcEnergy

  subroutine DoubleWell_initialize(nTotStep, initState, T, traj)
  use precision_m
  implicit none
  integer(kind=4), intent(in) :: nTotStep
  type(state_info) :: initState
  real(kind=fp_kind), intent(in) :: T
  real(kind=fp_kind) :: random_numbers(2*ntotstep)
  character(len=80) :: traj
  real(kind=fp_kind) :: myrand
  integer(kind=4) :: i
  nStep = nTotStep
  allocate(states(0:nStep))
  allocate(random_xdelta(nStep))
  allocate(random_exp(nStep))

  call calcEnergy(initState)

  states(0) = initState
  temperature = T
  trajfile = traj

  do i = 1, 2*ntotstep
    random_numbers(i) = myrand()
  end do

  random_xdelta = random_numbers(1:ntotstep) - 0.5d0
  random_exp = random_numbers(ntotstep+1:2*ntotstep)
  open(99,file=trajfile)
  end subroutine DoubleWell_initialize

  subroutine DoubleWell_finalize
  use precision_m
  implicit none
  deallocate(states)
  close(99)
  end subroutine DoubleWell_finalize

  subroutine MC_DoubleWell_run
  use precision_m
  implicit none
  type (state_info) :: stateCurrent, stateTrial
  integer(kind=4) :: istep
  real(kind=fp_kind) :: random_move, random_probability
  integer(kind=4) :: isuccess
  do istep = 1, nStep
    stateCurrent = states(istep-1)
    stateTrial%x = stateCurrent%x + random_xdelta(istep)
    call calcEnergy(stateTrial)
    random_probability = random_exp(istep)
    call MC_Propagate(stateCurrent, stateTrial, random_probability)
    states(istep) = stateCurrent
    write(99,'(I8,1X,F10.3,1X,F10.3,1X,I4)')istep, states(istep)%x, states(istep)%u, states(istep)%isucc
  end do
  end subroutine MC_DoubleWell_run

  subroutine MC_Propagate(stateCurrent, stateTrial, random_probability)
  use precision_m
  implicit none
  type (state_info), intent(in out) :: stateCurrent
  type (state_info), intent(in) :: stateTrial
  real(kind=fp_kind), intent(in) :: random_probability
 
  if ( stateTrial%u <= stateCurrent%u )then
    stateCurrent = stateTrial
    stateCurrent%isucc = 1
  else 
    if( exp(-(stateTrial%u-stateCurrent%u)/temperature) > random_probability ) then
      stateCurrent = stateTrial
      stateCurrent%isucc = 1
    else
      stateCurrent = stateCurrent
      stateCurrent%isucc = 0
    end if
  end if
  end subroutine MC_Propagate
end module MC_DoubleWell
