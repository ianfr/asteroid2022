module gravity_module
    use type_module
    implicit none
    real, parameter :: BIG_G = 6.67430e-11
contains

! calculate a list of gravitational forces on each particle from each particle
subroutine calculate_forces(particle_list, force_list)
    ! Input/Output variables
    type(particle), dimension(:), intent(inout) :: particle_list
    real, dimension(:,:), allocatable, intent(out) :: force_list

    ! Internal variables for gravity
    integer :: i, j
    real :: Gmm
    real, dimension(3) :: temp_force
    real, dimension(3) :: r_vector, r_unit_vector

    ! Internal variables for fictional collision force
    real :: m1, m2, m_eff, epsilon, v_imp, j_imp
    real, dimension(3) :: pos1, pos2, v1, v2
    real, dimension(3) :: n, scaled_force
    type(particle) :: part1, part2
    real :: beta, dt_coll

    allocate( force_list(3,size(particle_list)) )

    ! calculate the total force on each particle
    ! for gravity
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

    ! calculate the total force on each particle
    ! for fictional collision force
    do i = 1, size(particle_list), 1
        temp_force = 0
        do j = 1, size(particle_list), 1
            if (i .ne. j) then
                part1 = particle_list(i)
                part2 = particle_list(j)
                m1 = part1%mass
                m2 = part2%mass
                pos1 = part1%pos
                pos2 = part2%pos
                v1 = part1%vel
                v2 = part2%vel

                if (norm2(pos2-pos1) < 0.05) then
                    particle_list(i)%color = -1
                    epsilon = 1 ! coefficient of restitution
                    beta = 1 ! for making force vanish unless close
                    dt_coll = 1

                    n = (pos2 - pos1) / norm2(pos2 - pos1)
                    m_eff = 1.0 / ( (1.0/m1) + (1.0/m2) )
                    v_imp = dot_product(n, v1 - v2)
                    j_imp = (1 + epsilon) * m_eff * v_imp

                    ! scaled_force = exp(-beta * norm2(pos2 - pos1)) * ((1.0/dt_coll) * (1 + epsilon) * m_eff * v_imp * n)
                    scaled_force = ((1.0/dt_coll) * (1 + epsilon) * m_eff * v_imp * n)
                    temp_force = temp_force - scaled_force
                    ! if (norm2(temp_force) > 0.5) then
                    !     print*,"temp_force",temp_force
                    ! end if
                end if
            end if
        end do
        force_list(:,i) = force_list(:,i) + temp_force
    end do

end subroutine calculate_forces

! update the particles with motion uder gravity
! euler integration
! no COM speedups
subroutine gravity_update_euler(particle_list, timestep)
    ! Input variables
    type(particle), dimension(:), intent(inout) :: particle_list
    real, intent(in) :: timestep

    ! Internal variables
    integer :: i
    real, dimension(:,:), allocatable :: force_list

    ! subroutine

    ! force_list = calculate_forces(particle_list)
    call calculate_forces(particle_list, force_list)

    ! apply each calculated force to each particle for one euler step
    do i = 1, size(particle_list), 1
        particle_list(i)%vel = particle_list(i)%vel + (force_list(:,i) / particle_list(i)%mass) * timestep
        particle_list(i)%pos = particle_list(i)%pos + particle_list(i)%vel * timestep
    end do

end subroutine gravity_update_euler

end module gravity_module