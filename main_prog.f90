program main_prog

    ! IMPORTS
    ! use testfunction_module
    use asteroid_module
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
    integer :: i

    ! Variables for asteroid creation
    type(particle), allocatable :: particle_list(:)
    integer :: ast_ind
    integer :: num_asteroids

    ! Varibles that are hard-coded parameters for now
    real :: PARTICLE_RADIUS = 0.4

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

    !print*, ASTEROID_POSITIONS(:,2) ! second asteroid position

    ! create the asteroids
    ! print*, "[main_prog] adding asteroids..."
    ! num_asteroids = size(ASTEROID_MASSES)
    ! do ast_ind = 1, num_asteroids, 1
    !   call add_asteroid(particle_list, ASTEROID_MASSES(ast_ind), ASTEROID_RADII(ast_ind), &
    !     PARTICLE_RADIUS, ASTEROID_POSITIONS(:,ast_ind), ASTEROID_VELOCITIES(:,ast_ind), ast_ind) ! modifies particle_list
    ! end do
    ! print*, "[main_prog] DONE adding asteroids."
    ! print*, "number of particles: ", size(particle_list)

    !call write_particle_list_to_file(particle_list, "small_batch.txt")

    call read_particle_list_from_file(particle_list, "small_batch.txt")
    print*, (size(particle_list))

end program main_prog