function random_my result(ranval)
use precision_m
use IFPORT
implicit none
real(kind=fp_kind) :: ranval
call random_seed
call random_number(ranval)
end function random_my

