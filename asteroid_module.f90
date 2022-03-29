! based on https://gitlab.com/ifriedri/advanced-asteroid-sim/-/blob/master/aapopulation.h
module asteroid_module
    use type_module
    implicit none

    ! Custom asteroid coordinate type
    type asteroid_coordinate
        real, dimension(3) :: r
        logical valid
    end type asteroid_coordinate

contains 

subroutine print_grid(the_grid)
    type(asteroid_coordinate), dimension(:), intent(in) :: the_grid
    integer i
    do i = 1, size(the_grid), 1
        print*, the_grid(i)
    end do
end subroutine print_grid

subroutine push_ast_coord(the_array, the_item)
    ! VARIABLES
    type(asteroid_coordinate), allocatable, intent(inout) :: the_array(:)
    type(asteroid_coordinate), intent(in) :: the_item
    type(asteroid_coordinate), allocatable :: tmp_array(:)

    ! SUBROUTINE
    allocate(tmp_array(size(the_array) + 1))
    tmp_array(1:size(the_array)) = the_array
    tmp_array(size(tmp_array)) = the_item
    the_array = tmp_array ! req. fortran 2003?
end subroutine push_ast_coord

subroutine add_asteroid(particle_list, mass_asteroid, radius, particle_radius, r_, v_, color)
    ! Arguments
    type(particle), dimension(:), allocatable, intent(inout) :: particle_list
    real, intent(in) :: mass_asteroid
    real, intent(in) :: radius
    real, intent(in) :: particle_radius
    real, dimension(3), intent(in) :: r_
    real, dimension(3), intent(in) :: v_
    integer, intent(in) :: color 

    ! Internal variables
    type(asteroid_coordinate), dimension(:), allocatable :: asteroid_grid
    real :: buffer = 0.001 ! extra distance between particles in an asteroid
    real :: r_range
    real, dimension(3) :: r_mins, r_maxs
    type(asteroid_coordinate) :: temp
    real :: current_x, current_y, current_z
    integer :: num_invalid_points, num_valid_points
    integer :: i
    real, dimension(3) :: diff
    real :: coordinate_distance
    type(particle) :: temp_particle
    real :: particle_mass
    logical :: tmp_logical

    ! subroutine start
    r_range = 1.5 * radius
    r_mins = r_ - r_range
    r_maxs = r_ + r_range

    ! populate the asteroid grid, start in the bottom left-hand corner
    current_x = r_mins(1)
    current_y = r_mins(2)
    current_z = r_mins(3)
    do while(current_x < r_maxs(1))
        do while (current_y < r_maxs(2))
            do while (current_z < r_maxs(3))
                temp%r = (/ current_x, current_y, current_z /)
                temp%valid = .true.
                call push_ast_coord(asteroid_grid, temp)
                current_z = current_z + particle_radius + buffer
            end do
            current_z = r_mins(3)
            current_y = current_y + particle_radius + buffer
        end do
        current_y = r_mins(2)
        current_x = current_x + particle_radius + buffer
    end do

    ! find the valid coordinates
    num_invalid_points = 0
    do i = 1, size(asteroid_grid), 1
        diff = r_ - asteroid_grid(i)%r
        coordinate_distance = norm2(diff)
        if (coordinate_distance > radius) then
            asteroid_grid(i)%valid = .false.
            num_invalid_points = num_invalid_points + 1
        end if
    end do
    num_valid_points = size(asteroid_grid) - num_invalid_points

    ! add the particles to the list to be returned
    particle_mass = mass_asteroid / num_valid_points
    print*, "[add_asteroid] size of at grid"
    print*, size(asteroid_grid)
    do i = 1, size(asteroid_grid), 1
        tmp_logical = asteroid_grid(i)%valid
        if (tmp_logical) then
            temp_particle%pos = asteroid_grid(i)%r
            temp_particle%vel = v_
            temp_particle%mass = particle_mass
            temp_particle%radius = particle_radius
            temp_particle%color = color
            call push_particle(particle_list, temp_particle)
        end if
    end do

    do i = 1, size(particle_list) 
        particle_list(i)%id = i
    end do

    ! returns modified particle_list
end subroutine add_asteroid

end module asteroid_module