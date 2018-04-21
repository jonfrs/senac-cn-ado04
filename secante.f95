program secante
    implicit none
    character(len=10) :: arg
    integer :: iter = 1000
    real , parameter ::  epsilon = 0.001 
    real ::  fx=0,x, a, b,number, fa, fb  !f(x)=((x)*x - number'
                                    !flinha(x) = g(x) = x**3 - 2.0*sin(x)' 

    call getarg(1, arg)
    read (arg, '(f10.0)') number
    !do 20 i=1, 2, 1
    !20 continue

    a = 0
    b = number + 1

    fa = (a)*a - number
    fb = (b)*b - number
    x = fb - fa 

    do while (iter  >= 0)
        if (b /= a) then
            fx = b - (fb * (b-a))/x
        end if
        a = b
        b = fx
        fa = (a)*a - number
        fb = (b)*b - number
        x = fb - fa
        iter = iter -1
    end do
print '("A raiz é: "(f0.2))' , fx
end program secante