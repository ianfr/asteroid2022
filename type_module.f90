module type_module
    implicit none

    ! Basic particle type
    type particle
        integer :: id
        integer :: ast_id
        real :: mass
        real :: pos(3)
        real :: vel(3)
        real, allocatable :: grav_neighs(:) ! neighbor ids for gravity
    end type particle

contains

subroutine create_particle(theParticle)
    implicit none
    type(particle), intent(out) :: theParticle

    theParticle%id = 0
    theParticle%ast_id = 0
    theParticle%mass = 1
    theParticle%pos = [0,0,0]
    theParticle%vel = [0,0,0]
    theParticle%grav_neighs = [0]
end subroutine

end module type_module 
