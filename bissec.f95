program bissec
    implicit none
    character(len=32) :: arg
    integer :: i
    real , parameter ::  epsilon = 0.001
    real ::  fx,x, a, b,number, fa, fb  !f(x)=((x)*x - number'
                                    !flinha(x) = g(x) = x**3 - 2.0*sin(x)' 

    call getarg(1, arg)
    read (arg, *) number
    !do 20 i=1, 2, 1
    !20 continue

    a = 1
    b = (number +1) / 2

    fa = (a)*a - number
    fb = (b)*b - number
    i=0


    if (fa * fb < 0) then
        x = (a + b) / 2
        fx = (x)*x - number
            do while (b - a > epsilon)
                i = i+1
                if  (fa * fx >= 0) then
                    a = x
                else
                    b = x
                end if
                x = (a+b) / 2
                fx = ((x)*x) - number
                fa = (a)*a - number
            end do
            print '("A raiz é: "(f0.2))' , x
    else
        write(*,*)"Não foi possivel achar a raiz"
    end if
end program bissec