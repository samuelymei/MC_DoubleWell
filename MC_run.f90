program MC_run
  use precision_m
  use MC_DoubleWell
  implicit none
  integer(kind=4) :: nTotStep
  real(kind=fp_kind) :: T
  type(state_info) :: initState
  character(len=80) :: traj
  print*, 'Input number of steps of propagation'
  read*,nTotStep
  print*, 'Input initial position'
  read*,initState%x
  print*, 'Input temperature'
  read*,T
  print*, 'Input the name of the trajectory file'
  read*,traj
  call DoubleWell_initialize(nTotStep, initState, T, traj)
end program MC_run
