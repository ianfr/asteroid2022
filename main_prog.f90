program main_prog

    ! IMPORTS
    use asteroid_module
    use type_module
    use readparams_module
    use utilities_module
    use gravity_module
    use collision_module

    ! VARIABLES
    implicit none
    ! type(particle) :: tmp_particle
    ! real, parameter :: PI = 4.0*ATAN(1.0)
    ! integer, parameter :: X = 1
    ! integer, parameter :: Y = 2
    ! integer, parameter :: Z = 3
    integer :: i
    real :: total_time, accum_coll_time

    ! Variables for asteroid creation
    type(particle), allocatable :: particle_list(:)
    integer :: ast_ind
    integer :: num_asteroids

    ! Varibles that are hard-coded parameters for now
    real :: PARTICLE_RADIUS
    real :: DT

    ! Variables for program parameters
    character(len=*), parameter :: PARAM_FILE_NAME = "control.txt"
    character(len=:), allocatable :: OUT_DIR
    integer :: NUM_PARTICLES, NUM_TIMESTEPS
    real :: MAX_TIME
    ! for now, we just use 2 asteroids
    real, dimension(:), allocatable :: ASTEROID_MASSES
    real, dimension(:), allocatable :: ASTEROID_RADII
    real, dimension(:,:), allocatable :: ASTEROID_POSITIONS ! every column is the initial asteroid position
    real, dimension(:,:), allocatable :: ASTEROID_VELOCITIES ! every column is the initial asteroid velocity

    ! type(collision_struct) :: next_coll

    ! PROGRAM START
    print*, "SIMULATION OF RUBBLE-PILE ASTEROIDS WITH:"
    print*, "    FORCES:"
    print*, "        GRAVITATIONAL"
    print*, "        FICTIONAL COLLISION"
    print*, "    INTEGRATION SCHEME OPTIONS:"
    print*, "        EULER"
    print*, ""
    print*, "(c) Ian Friedrichs 2022"
    print*, ""

    ! call create_particle(tmp_particle)
    
    ! print*, tmp_particle%mass!

    ! call readparams(PARAM_FILE_NAME, NUM_PARTICLES, NUM_TIMESTEPS, &
    !   MAX_TIME, ASTEROID_MASSES, ASTEROID_RADII, ASTEROID_POSITIONS, ASTEROID_VELOCITIES)

    call read_params_from_file(PARAM_FILE_NAME, NUM_PARTICLES, NUM_TIMESTEPS, &
      MAX_TIME, ASTEROID_MASSES, ASTEROID_RADII, ASTEROID_POSITIONS, ASTEROID_VELOCITIES, &
      DT, PARTICLE_RADIUS, OUT_DIR)

    ! print*, "Asteroid Simulation"
    ! print*, "n=",NUM_PARTICLES ! the maximum number - actual will be much less
    ! print*, "steps=",NUM_TIMESTEPS
    ! print*, "time=",MAX_TIME

    ! create the asteroids
    print*, "[main_prog] adding asteroids..."
    num_asteroids = size(ASTEROID_MASSES)
    do ast_ind = 1, num_asteroids, 1
      call add_asteroid(particle_list, ASTEROID_MASSES(ast_ind), ASTEROID_RADII(ast_ind), &
        PARTICLE_RADIUS, ASTEROID_POSITIONS(:,ast_ind), ASTEROID_VELOCITIES(:,ast_ind), ast_ind) ! modifies particle_list
    end do
    print*, "[main_prog] DONE adding asteroids."
    print*, "number of particles: ", size(particle_list)

    total_time = 0.0
    accum_coll_time = 0.0
    call write_particle_list_for_paraview(particle_list, OUT_DIR, 0)
    i = 1 ! keep track of number of timesteps passed (counts contribs from colls)
    do while (total_time < NUM_TIMESTEPS * DT .and. total_time < MAX_TIME)
      ! next_coll = get_next_collision(particle_list)
      ! if (next_coll%collision_time < 0 .or. next_coll%collision_time > DT) then
      !   call gravity_update_euler(particle_list, DT)
      !   total_time = total_time + DT
      !   i = i + 1
      !   print*, "Completed timestep", i, "of", NUM_TIMESTEPS, "with gravity"
      !   call write_particle_list_for_paraview(particle_list, OUT_DIR, i)
      ! else
      !   ! fast-forward to collision time, then perform collision
      !   ! print*, "Next coll in ", next_coll%collision_time
      !   call fast_forward(particle_list, next_coll%collision_time)
      !   call collide_wrapper(particle_list, next_coll)
      !   accum_coll_time = accum_coll_time + next_coll%collision_time
      !   total_time = total_time + next_coll%collision_time
      !   if (accum_coll_time > DT) then
      !     i = i + 1
      !     accum_coll_time = 0.0
      !     print*, "Completed timestep", i, "of", NUM_TIMESTEPS, "with fast-forward"
      !     call write_particle_list_for_paraview(particle_list, OUT_DIR, i)
      !   end if
      ! end if
      call gravity_update_euler(particle_list, DT)
      total_time = total_time + DT
      i = i + 1
      !print*, "Completed timestep", i, "of", NUM_TIMESTEPS, "with gravity & fictional coll. force"
      call write_particle_list_for_paraview(particle_list, OUT_DIR, i)
    end do
end program main_prog
