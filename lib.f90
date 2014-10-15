!subroutine random_my(n, ranval)
!use precision_m
!use IFPORT
!implicit none
!integer(kind=4), intent(in) :: n
!real(kind=fp_kind), intent(out) :: ranval(n)
!call random_seed
!call random_number(ranval)
!end subroutine random_my

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

function myrand()
use precision_m
implicit none
integer(kind=4),save::initialized=0
real(kind=fp_kind)::myrand
real(kind=fp_kind)::r
if(initialized.eq.0)then
  CALL init_random_seed()         ! see example of RANDOM_SEED
  initialized=1
end if
CALL RANDOM_NUMBER(r)
myrand=r
end function myrand

SUBROUTINE init_random_seed()
implicit none
INTEGER :: i, n, clock
INTEGER, DIMENSION(:), ALLOCATABLE :: seed
CALL RANDOM_SEED(size = n)
ALLOCATE(seed(n))
CALL SYSTEM_CLOCK(COUNT=clock)
seed = clock + 37 * (/ (i - 1, i = 1, n) /)
CALL RANDOM_SEED(PUT = seed)
DEALLOCATE(seed)
END SUBROUTINE init_random_seed
