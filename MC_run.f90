program MC_run
use precision_m
use MC_DoubleWell
implicit none
integer(kind=4) :: ntotstep
real(kind=fp_kind) :: x0
real(kind=fp_kind) :: T
character(len=80) :: traj
print*, 'Input number of steps of propagation'
read*,ntotstep
print*, 'Input initial position'
read*,x0
print*, 'Input temperature'
read*,T
print*, 'Input the name of the trajectory file'
read*,traj
call DoubleWell_initialize(ntotstep, x0, T, traj)
call MC_DoubleWell_start
call DoubleWell_finalize
end program MC_run
