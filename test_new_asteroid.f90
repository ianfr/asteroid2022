program test_new_asteroid

    ! IMPORTS
    use asteroid_module

    ! VARIABLES
    type(particle), allocatable :: particle_list(:)

    ! PROGRAM
    call add_asteroid_ellipsoid(particle_list, 1.0, 3.0, 3.0, 2.0, 0.05, [1.0,1.0,1.0], [0.0,0.0,0.0], 100000, 0)

    call write_particle_list_for_paraview(particle_list,"ellipse",0)

end program test_new_asteroid