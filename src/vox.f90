module vox_mod
    implicit none

    type :: vox
        integer :: xsize, ysize, zsize, nmodels
        integer, allocatable :: grid(:,:,:)
        logical :: extended
        contains
        procedure :: read => read_vox
    end type vox

    type :: chunk_t
        character(len=:), allocatable :: chunk_id
        integer :: N, M
        contains
        procedure :: read => read_chunk_from_file
    end type chunk_t

    interface read_nbytes
        module procedure read_nbytes_from_file
    end interface read_nbytes

contains

    subroutine read_vox(this, filename)

        implicit none

        class(vox) :: this
        character(len=*) :: filename
        character(len=:), allocatable :: bytes
        integer :: u, offset, numVoxels, i, j,x,y,z, gsize
        type(chunk_t) :: chunk
        logical :: flag

        open(newunit=u, file=filename, access="stream", status="OLD", form="unformatted")
        call read_magic(u, offset, flag)
        this%extended = flag

        gsize = 1
        if(flag)gsize = 4

        call chunk%read(u, offset)
        if(chunk%chunk_id /= "MAIN")error stop "No MAIN chunk!"

        call chunk%read(u, offset)
        if(chunk%chunk_id == "PACK")then
            call read_nbytes(u, 4, offset, bytes) 
            this%nmodels = ichar(bytes)
            call chunk%read(u, offset)
        else
            this%nmodels = 1
        end if

        do i = 1, this%nmodels
            if(chunk%chunk_id == "SIZE")then
                call read_nbytes(u, 4, offset, bytes)
                this%xsize = transfer(bytes, z)
                
                call read_nbytes(u, 4, offset, bytes)
                this%ysize = transfer(bytes, z)

                call read_nbytes(u, 4, offset, bytes)
                this%zsize = transfer(bytes, z)
                allocate(this%grid(0:this%xsize-1, 0:this%ysize-1, 0:this%zsize-1))
                this%grid = 0
                print*,this%xsize,this%ysize,this%zsize
            else
                error stop "Missing SIZE chunk!"
            end if

            call chunk%read(u, offset)
            if(chunk%chunk_id == "XYZI")then
                call read_nbytes(u, 4, offset, bytes)
                numVoxels = transfer(bytes, z)
                do j = 1, numVoxels

                    call read_nbytes(u, gsize, offset, bytes)
                    if(this%extended)then
                        x = transfer(bytes, z)
                    else
                        x = ichar(bytes)
                    end if
                    call read_nbytes(u, gsize, offset, bytes)
                    if(this%extended)then
                        y = transfer(bytes, z)
                    else
                        y = ichar(bytes)
                    end if
                    call read_nbytes(u, gsize, offset, bytes)
                    if(this%extended)then
                        z = transfer(bytes, z)
                    else
                        z = ichar(bytes)
                    end if
                    call read_nbytes(u, 1, offset, bytes)
                    ! print*,x,y,z, ichar(bytes)
                    this%grid(x, y, z) = ichar(bytes)
                end do
            else
                error stop "Missing XYZI chunk!"
            end if
        end do
    end subroutine read_vox


    subroutine read_magic(u, offset, flag)
        
        implicit none
    
        integer, intent(IN)  :: u
        integer, intent(OUT) :: offset
        logical, intent(OUT) :: flag

        character(len=:), allocatable :: bytes
        
        offset = 1
        call read_nbytes(u, 4, offset, bytes)
        if(bytes /= "VOX ")error stop "Not a VOX file!"
        call read_nbytes(u, 4, offset, bytes)
        flag = .false.
        if(ichar(bytes) /= 150)then
            if(ichar(bytes) /= 160)then
                error stop "Not a valid VOX file!"
            else
                flag = .true.
            end if
        end if

    end subroutine read_magic
    

    subroutine read_chunk_from_file(this, u, offset)
        
        use iso_fortran_env, only : int32

        implicit none
    
        class(chunk_t) :: this
        integer :: u, offset
        integer(kind=int32) :: mold
        character(len=:), allocatable :: chunk_id, bytes

        call read_nbytes(u, 4, offset, chunk_id)
        this%chunk_id = chunk_id
        
        call read_nbytes(u, 4, offset, bytes)
        this%N = transfer(bytes, mold)

        call read_nbytes(u, 4, offset, bytes)
        this%M = transfer(bytes, mold)

    end subroutine read_chunk_from_file

    subroutine read_nbytes_from_file(u, nbytes, offset, bytes)
        
        implicit none
        
        integer :: u, nbytes, offset
        character(len=:), allocatable :: bytes

        character(len=1) :: byte
        integer :: i

        bytes = ""

        do i = 1, nbytes
            read(u, pos=offset) byte
            offset = offset + 1
            bytes = bytes // byte
        end do

    end subroutine read_nbytes_from_file
end module vox_mod