module m
    implicit none

    type :: vox
        integer :: xsize, ysize, zsize, nmodels
        integer, allocatable :: grid(:,:,:)
        contains
        procedure :: read => read_vox
    end type vox

    type :: chunk_t
        character(len=:), allocatable :: chunk_id, content
        integer :: N, M
        contains
        procedure :: read_chunk_from_chars, read_chunk_from_file
        generic :: read => read_chunk_from_file, read_chunk_from_chars
    end type chunk_t

    interface read_nbytes
        module procedure read_nbytes_from_file
        module procedure read_nbytes_from_char
    end interface read_nbytes

contains


    subroutine read_vox(this, filename)

        implicit none

        class(vox) :: this
        character(len=*) :: filename
        character(len=:), allocatable :: bytes, byte
        character(len=1) :: mold
        integer :: u, offset, numVoxels, i, j,x,y,z
        type(chunk_t) :: chunk

        open(newunit=u, file=filename, access="stream", form="unformatted")
        call read_magic(u)

        call chunk%read(u)
        close(u)
        if(chunk%chunk_id /= "MAIN")error stop "No MAIN chunk!"

        offset = 1
        bytes = chunk%content
        call chunk%read(bytes, offset)
        if(chunk%chunk_id == "PACK")then
            this%nmodels = ichar(chunk%content)
            call chunk%read(bytes, offset)
        else
            this%nmodels = 1
        end if

        do i = 1, this%nmodels
            if(chunk%chunk_id == "SIZE")then
                call read_nbytes(chunk%content, 1, 4, byte)
                this%xsize = ichar(byte)
                
                call read_nbytes(chunk%content, 5, 4, byte)
                this%ysize = ichar(byte)

                call read_nbytes(chunk%content, 9, 4, byte)
                this%zsize = ichar(byte)
                allocate(this%grid(0:this%xsize-1, 0:this%ysize-1, 0:this%zsize-1))
                this%grid = 0
                print*,this%xsize,this%ysize,this%zsize
            else
                error stop "Missing SIZE chunk!"
            end if

            call chunk%read(bytes, offset)
            if(chunk%chunk_id == "XYZI")then
                offset = 1
                call read_nbytes(chunk%content, offset, 4, byte)
                offset = offset + 4
                numVoxels = transfer(byte, z)
                do j = 1, numVoxels
                    call read_nbytes(chunk%content, offset, 1, byte)
                    x = ichar(byte)
                    offset = offset + 1

                    call read_nbytes(chunk%content, offset, 1, byte)
                    y = ichar(byte)
                    offset = offset + 1

                    call read_nbytes(chunk%content, offset, 1, byte)
                    z = ichar(byte)
                    offset = offset + 1

                    call read_nbytes(chunk%content, offset, 1, byte)
                    ! print*,x,y,z
                    this%grid(x, y, z) = ichar(byte)
                    offset = offset + 1
                end do
            else
                error stop "Missing XYZI chunk!"
            end if
        end do
    end subroutine read_vox


    subroutine read_magic(u)
        
        implicit none
    
        integer, intent(IN) :: u
        character(len=:), allocatable :: bytes

        call read_nbytes(u, 4, bytes)
        if(bytes /= "VOX ")error stop "Not a VOX file!"
        call read_nbytes(u, 4, bytes)
        if(ichar(bytes) /= 150)error stop "Not a valid VOX file!"
    
    end subroutine read_magic
    

    subroutine read_chunk_from_file(this, u)
        
        implicit none
    
        class(chunk_t) :: this
        integer :: u
        character(len=:), allocatable :: chunk_id, chunk, bytes
        integer :: N, M

        call read_nbytes(u, 4, chunk_id)
        this%chunk_id = chunk_id
        
        call read_nbytes(u, 4, bytes)
        this%N = transfer(bytes, u)

        call read_nbytes(u, 4, bytes)
        this%M = transfer(bytes,u)

        call read_nbytes(u, this%N + this%M, bytes)
        this%content = bytes

    end subroutine read_chunk_from_file


    subroutine read_chunk_from_chars(this, chars, offset)
        
        implicit none
    
        class(chunk_t) :: this
        integer :: offset
        character(len=*) :: chars
        character(len=:), allocatable :: chunk_id, chunk, bytes
        character(len=1) :: byte
        integer :: N, M

        call read_nbytes(chars, offset, 4, chunk_id)
        offset = offset + 4
        this%chunk_id = chunk_id
        
        call read_nbytes(chars, offset, 4, bytes)
        offset = offset + 4
        this%N = transfer(bytes, offset)

        call read_nbytes(chars, offset, 4, bytes)
        offset = offset + 4
        this%M = transfer(bytes,offset)

        call read_nbytes(chars, offset, this%N + this%M, bytes)
        offset = offset + this%N + this%M
        this%content = bytes

    end subroutine read_chunk_from_chars


    subroutine read_nbytes_from_char(string, offset, nbytes, bytes)
        
        implicit none
        
        integer :: nbytes, offset
        character(len=*) :: string
        character(len=:), allocatable :: bytes

        bytes = string(offset:offset+nbytes-1)

    end subroutine read_nbytes_from_char


    subroutine read_nbytes_from_file(u, nbytes, bytes)
        
        implicit none
        
        integer :: u, nbytes
        character(len=:), allocatable :: bytes

        character(len=1) :: byte
        integer :: i

        bytes = ""

        do i = 1, nbytes
            read(u) byte
            bytes = bytes // byte
        end do

    end subroutine read_nbytes_from_file
    
end module m
program p

    use m

    implicit none
    
    character(len=:), allocatable :: filename
    integer :: u
    type(vox) :: voxf

    filename = "../vox/test.vox"!"Downloads/MagicaVoxel-0.99.6.4-win64/vox/castle.vox"
    call voxf%read(filename)

    ! open(newunit=u,file="castle.raw", access="stream", form="unformatted")
    ! write(u)real(voxf%grid)
    ! close(u)

end program p