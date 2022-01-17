program p

    use vox_mod

    implicit none
    
    character(len=:), allocatable :: filename
    integer :: u
    type(vox) :: voxf

    filename = "../cone.vox"
    call voxf%read(filename)

    open(newunit=u,file="test.raw", access="stream", form="unformatted")
    write(u)real(voxf%grid)
    close(u)

end program p