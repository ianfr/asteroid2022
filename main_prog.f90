program main_prog

    ! IMPORTS
    ! use testfunction_module
    use type_module
    use readparams_module
    use utilities_module

    ! VARIABLES
    implicit none
    type(particle) :: tmp_particle
    real, parameter :: PI = 4.D0*DATAN(1.D0)
    integer, parameter :: X = 1
    integer, parameter :: Y = 2
    integer, parameter :: Z = 3

    ! Variables for asteroid creation
    type(particle), allocatable :: particle_list(:)
    integer :: astInd
    integer :: numAsteroids
    integer :: actual_num_particles = 0
    real :: total_volume = 0.0
    real :: spacing
    real, allocatable :: tmp_grid(:,:)

    ! Variables for program parameters
    character(len=*), parameter :: PARAM_FILE_NAME = "params.txt"
    integer :: NUM_PARTICLES, NUM_TIMESTEPS
    real :: MAX_TIME
    ! for now, we just use 2 asteroids
    real, dimension(:), allocatable :: ASTEROID_MASSES
    real, dimension(:), allocatable :: ASTEROID_RADII
    real, dimension(:,:), allocatable :: ASTEROID_POSITIONS ! every column is the initial asteroid position
    real, dimension(:,:), allocatable :: ASTEROID_VELOCITIES ! every column is the initial asteroid velocity

    ! PROGRAM START
    call create_particle(tmp_particle)
    
    print*, tmp_particle%mass

    call readparams(PARAM_FILE_NAME, NUM_PARTICLES, NUM_TIMESTEPS, &
      MAX_TIME, ASTEROID_MASSES, ASTEROID_RADII, ASTEROID_POSITIONS, ASTEROID_VELOCITIES)

    print*, "Asteroid Simulation"
    print*, "n=",NUM_PARTICLES ! the maximum number - actual will be much less
    print*, "steps=",NUM_TIMESTEPS
    print*, "time=",MAX_TIME

    print*, ASTEROID_POSITIONS(:,2) ! second asteroid position

    ! Count how many particles we're gonna need to avoid reallocation

    numAsteroids = size(ASTEROID_MASSES)

    ! calculate the total volume that needs to be filled with particles
    ! this is not the actual volume, but the approximate rectangular volume to make grid
    ! do astInd = 1, numAsteroids, 1
    !   total_volume = total_volume + ( (4.0/3.0) * PI * (ASTEROID_RADII(astInd)**3) )
    ! end do
    do astInd = 1, numAsteroids, 1
      total_volume = total_volume + ASTEROID_RADII(astInd)**3
    end do
    print*, "Total volume=", total_volume

    ! calculate grid points
    spacing = (total_volume / NUM_PARTICLES)**(1.0/3.0)
    do astInd = 1, numAsteroids, 1

      ! count up how many of the grid points fall into the spheres
      
    end do

    ! allocate the list of all particles

    ! now, create the actual particles for each asteroid
    !do astInd = 1,numAsteroids,1
      
    !end do

end program main_prog