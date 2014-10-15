subroutine random_my(n, ranval)
use precision_m
use IFPORT
implicit none
integer(kind=4), intent(in) :: n
real(kind=fp_kind), intent(out) :: ranval(n)
call random_seed
call random_number(ranval)
end subroutine random_my

subroutine sort( n, array ) 
  use precision_m
  implicit none
  integer(kind=4), intent(in) :: n
  real(kind=fp_kind), intent(in out) :: array(n)
  integer(kind=4) :: swapped
  real(kind=fp_kind) :: temp
  integer(kind=4) :: i, j, k
  swapped = 1
  do j = n-1, 1, -1
    swapped = 0
    do i = 1, j
      if(array(i) > array(i+1))then
        temp = array(i)
        array(i) = array(i+1)
        array(i+1) = temp
        swapped = 1
      end if
    end do
    if(swapped == 0) exit
  end do
end subroutine sort

