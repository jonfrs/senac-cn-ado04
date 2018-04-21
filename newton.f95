program newton
    implicit none
    character(len=32) :: arg
    integer :: iter = 100
    real ::  fx, number,x
    call getarg(1, arg)
    read (arg, '(f10.0)') number
    !do 20 i=1, 2, 1
    !20 continue
    fx = number /2 
    !fx = number

    do while (iter  >= 0)
       fx = (fx + (number / fx))/2
       !fx =  fx - ( (fx*fx) / (fx * 2) )

        iter = iter - 1
    end do
print '("A raiz é: "(f0.2))' , fx
end program newton