module readparams_module
    implicit none

contains

subroutine readparams(filename, numParticles, numTimesteps, maxTime, asteroidMasses, asteroidRadii, &
    asteroidPositions, asteroidVelocities)

    ! VARIABLES
    ! Variables for calling the subroutine
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    character(len=*), intent(in) :: filename
    integer, intent(out) :: numParticles, numTimesteps
    real, intent(out) :: maxTime
    real, dimension(:), allocatable, intent(out) :: asteroidMasses ! n masses
    real, dimension(:), allocatable, intent(out) :: asteroidRadii ! the radii of each asteroid
    real, dimension(:,:), allocatable, intent(out) :: asteroidPositions ! 3 rows, n columns
    real, dimension(:,:), allocatable, intent(out) :: asteroidVelocities ! 3 rows, n columns

    ! Variables for use within the subroutine
    integer :: file_unit
    integer :: rc
    character(len=5) ast_m_file, ast_rad_file, ast_pos_file, ast_vel_file
    integer :: n
    integer :: i
    real, dimension(:), allocatable :: temp3vector

    ! SUBROUTINE START
    ! Used https://cyber.dabamos.de/programming/modernfortran/files.html as reference

    open(action='read', file=filename, iostat=rc, newunit=file_unit)

    if (rc /= 0) then
        write (error_unit, '(3a, i0)') 'Reading file "', filename, '" failed: ', rc
        stop
    end if

    do
        read (file_unit, *, iostat=rc) numParticles, numTimesteps, maxTime, n, ast_m_file, ast_rad_file, &
            ast_pos_file, ast_vel_file
        if (rc /= 0) exit                           
    end do

    close (file_unit)

    ! now, read in the matrices using the filenames stored in ast_[etc]
    ! used https://shocksolution.com/2010/05/21/reading-an-array-from-a-text-file-with-fortran-9095/

    open (unit=90, file=ast_m_file, status='old', action='read')
    allocate(asteroidMasses(n))
    read(90,*) asteroidMasses
    close(90)

    open (unit=90, file=ast_rad_file, status='old', action='read')
    allocate(asteroidRadii(n))
    read(90,*) asteroidRadii
    close(90)

    ! the matrices in the files get transposed since fortran is column-major in memory
    allocate(temp3vector(3))
    open (unit=91, file=ast_pos_file, status='old', action='read')
    allocate(asteroidPositions(3,n))
    do i=1,n,1
        read(91,*) temp3vector
        asteroidPositions(:, i) = temp3vector
    end do
    close(91)

    open (unit=92, file=ast_vel_file, status='old', action='read')
    allocate(asteroidVelocities(3,n))
    do i=1,n,1
        read(92,*) temp3vector
        asteroidVelocities(:, i) = temp3vector
    end do
    close(92)

end subroutine

end module readparams_module

