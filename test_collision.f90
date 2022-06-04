program test_collision
    ! IMPORTS
    use collision_module

    ! VARIABLES
    type(particle), allocatable :: particle_list(:)

    ! PROGRAM
    allocate(particle_list(2))
    call create_particle(particle_list(1))
    call create_particle(particle_list(2))

    particle_list(1)%pos = (/ 1.0, 1.0, 1.0 /)
    particle_list(1)%vel = (/ 1.0, 0.0, 0.0 /)
    particle_list(2)%pos = (/ 2.05, 1.0, 1.0 /)
    particle_list(2)%vel = (/ -1.0, 0.0, 0.0 /)

    call collide_old(particle_list(1), particle_list(2))

    print*, "old: "
    call print_particle(particle_list(1))
    call print_particle(particle_list(2))

    particle_list(1)%pos = (/ 1.0, 1.0, 1.0 /)
    particle_list(1)%vel = (/ 1.0, 0.0, 0.0 /)
    particle_list(2)%pos = (/ 2.05, 1.0, 1.0 /)
    particle_list(2)%vel = (/ -1.0, 0.0, 0.0 /)

    call collide(particle_list(1), particle_list(2))

    print*, "new: "
    call print_particle(particle_list(1))
    call print_particle(particle_list(2))

end program test_collision
