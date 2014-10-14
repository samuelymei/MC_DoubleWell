program r
use IFPORT
implicit none
real(kind=8) ranval
call random_seed
call random_number(ranval)
print *, ranval
end program r
