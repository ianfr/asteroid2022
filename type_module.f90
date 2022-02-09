module type_module
    implicit none

    ! Basic particle type
    type particle
        integer :: id
        integer :: ast_id
        integer :: color
        real :: mass
        real :: radius
        real :: pos(3)
        real :: vel(3)
        real, allocatable :: grav_neighs(:) ! neighbor ids for gravity
    end type particle

contains

subroutine print_particle(the_particle)
    type(particle), intent(in) :: the_particle

    print*, "PARTICLE START"
    print*, "id:"
    print*, the_particle%id
    print*, "mass:"
    print*, the_particle%mass
    print*, "position:"
    print*, the_particle%pos
    print*, "velocity:"
    print*, the_particle%vel
    print*, "PARTICLE END"
end subroutine print_particle

subroutine create_particle(theParticle)
    implicit none
    type(particle), intent(out) :: theParticle

    theParticle%id = 0
    theParticle%ast_id = 0
    theParticle%color = 0
    theParticle%mass = 1
    theParticle%radius = 1
    theParticle%pos = [0,0,0]
    theParticle%vel = [0,0,0]
    theParticle%grav_neighs = [0]
end subroutine

subroutine push_particle(the_array, the_item)
    ! VARIABLES
    type(particle), allocatable, intent(inout) :: the_array(:)
    type(particle), intent(in) :: the_item
    type(particle), allocatable :: tmp_array(:)

    ! SUBROUTINE
    allocate(tmp_array(size(the_array) + 1))
    tmp_array(1:size(the_array)) = the_array
    tmp_array(size(tmp_array)) = the_item
    the_array = tmp_array ! req. fortran 2003?
end subroutine push_particle

end module type_module 
