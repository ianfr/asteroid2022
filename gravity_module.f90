module gravity_module
    use type_module
    implicit none
    real, parameter :: BIG_G = 6.67430e-11
    ! real, parameter :: BIG_G = 6.67430e-1
contains

! update the particles with motion uder gravity
! euler integration
! no COM speedups
subroutine gravity_update_euler(particle_list, timestep)
    ! Input variables
    type(particle), dimension(:), intent(inout) :: particle_list
    real, intent(in) :: timestep

    ! Internal variables
    integer :: i, j
    real :: Gmm
    real, dimension(3) :: temp_force
    real, dimension(3) :: r_vector, r_unit_vector
    real, dimension(:,:), allocatable :: force_list

    ! subroutine

    allocate( force_list(3,size(particle_list)) )

    ! calculate the total force on each particle
    do i = 1, size(particle_list), 1
        temp_force = 0
        do j = 1, size(particle_list), 1
            if (i .ne. j) then
                Gmm = BIG_G * particle_list(i)%mass  * particle_list(j)%mass
                r_vector = particle_list(i)%pos - particle_list(j)%pos
                r_unit_vector = r_vector / norm2(r_vector)
                temp_force = temp_force - 1.0 * (Gmm * r_unit_vector) / norm2(r_vector)**2 
            end if
        end do
        force_list(:,i) = temp_force
    end do

    ! apply each calculated force to each particle for one euler step
    do i = 1, size(particle_list), 1
        particle_list(i)%vel = particle_list(i)%vel + (force_list(:,i) / particle_list(i)%mass) * timestep
        particle_list(i)%pos = particle_list(i)%pos + particle_list(i)%vel * timestep
    end do

end subroutine gravity_update_euler

end module gravity_module