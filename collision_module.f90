! based on https://gitlab.com/ifriedri/advanced-asteroid-sim/-/blob/master/aaparticle.h
module collision_module
    use type_module
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none

contains

function calculate_collision_time(part1, part2) result(time_till_collision)
    ! Function Inputs / Outputs
    type(particle), intent(in) :: part1, part2
    real :: time_till_collision

    ! Local Variables
    real :: b, u, dist, d, discr

    ! Function
    b = dot_product((part2%pos - part1%pos), (part2%vel - part1%vel))
    u = norm2(part2%vel - part1%vel)
    dist = norm2(part1%pos - part2%pos)
    d = 2.0 * part1%radius ! MIGHT CHANGE TO P1%RADIUS + P2%RADIUS WHEN NON EQUAL RADII ARE USED
    discr = b*b - u*u *(dist*dist - d*d)

    time_till_collision = (-1.0*b - sqrt(discr)) / (u*u)

end function

! adapted from https://physics.stackexchange.com/questions/681396/elastic-collision-3d-eqaution
subroutine collide_old(part1, part2)
    type(particle), intent(inout) :: part1, part2

    real, dimension(3) :: r1, r2, r12, r21, r12_hat, r21_hat
    real, dimension(3) :: v1, v2, v12, v21
    real :: mass_factor_1, mass_factor_2

    v1 = part1%vel
    v2 = part2%vel
    v12 = v1 - v2
    v21 = v2 - v1

    r1 = part1%pos
    r2 = part2%pos
    r12 = r1 - r2
    r21 = r2 - r1
    r12_hat = r12/norm2(r12)
    r21_hat = r21/norm2(r21)

    mass_factor_1 = (2*part2%mass) / (part1%mass + part2%mass)
    mass_factor_2 = (2*part1%mass) / (part2%mass + part1%mass)

    part1%vel = part1%vel - mass_factor_1 * dot_product(r12_hat, v12) * r12_hat

    part2%vel = part2%vel - mass_factor_2 * dot_product(r21_hat, v21) * r21_hat

end subroutine

! adapted from https://physics.stackexchange.com/questions/681396/elastic-collision-3d-eqaution
subroutine collide(part1, part2)
    type(particle), intent(inout) :: part1, part2

    real :: m1, m2, m_eff, epsilon, v_imp, j
    real, dimension(3) :: pos1, pos2, v1, v2
    real, dimension(3) :: n, dv1, dv2

    m1 = part1%mass
    m2 = part2%mass
    pos1 = part1%pos
    pos2 = part2%pos
    v1 = part1%vel
    v2 = part2%vel

    epsilon = 1 ! coeff of restitution

    n = (pos2 - pos1) / norm2(pos2 - pos1)
    m_eff = 1.0 / ( (1.0/m1) + (1.0/m2) )
    v_imp = dot_product(n, v1 - v2)
    j = (1 + epsilon) * m_eff * v_imp

    dv1 = -(j/m1) * n
    dv2 =  (j/m2) * n
    
    part1%vel = part1%vel + dv1
    part2%vel = part2%vel + dv2

end subroutine

subroutine collide_wrapper(particle_list, next_coll)
    type(particle), dimension(:), intent(inout) ::particle_list
    type(collision_struct), intent(in) :: next_coll

    ! local variables
    integer ind_1, ind_2, p_ind_1, p_ind_2, i

    ind_1 = next_coll%particle_ids(1)
    ind_2 = next_coll%particle_ids(2)

    do i = 1, size(particle_list), 1
        if (particle_list(i)%id .eq. ind_1) then
            p_ind_1 = i
        end if
        if (particle_list(i)%id .eq. ind_2) then
            p_ind_2 = i
        end if
    end do

    call collide(particle_list(p_ind_1), particle_list(p_ind_2))

end subroutine collide_wrapper


! assumes that collision_list is already allocated
subroutine calculate_all_collisions(particle_list, collision_list)
    ! Function Inputs/Outputs
    type(particle), dimension(:), intent(in) :: particle_list
    type(collision_struct), dimension(:), intent(inout) :: collision_list ! size n^2 - n

    ! Local Variables
    integer :: i, j, c

    ! Subroutine
    c = 1
    do i = 1, size(particle_list), 1
        do j = 1, size(particle_list), 1
            if (i .ne. j) then
                collision_list(c)%particle_ids = (/ particle_list(i)%id, particle_list(j)%id /)
                collision_list(c)%collision_time = abs(calculate_collision_time(particle_list(i), particle_list(j)))
                if (.not. ieee_is_finite(collision_list(c)%collision_time)) then
                    collision_list(c)%collision_time = 1e7
                end if
                c = c + 1
            end if
        end do
    end do

end subroutine calculate_all_collisions

function get_next_collision(particle_list) result(next_coll)
    type(particle), dimension(:), intent(in) :: particle_list
    type(collision_struct) :: next_coll

    ! Local variables
    type(collision_struct), dimension(:), allocatable :: collision_list ! size n^2 - n
    integer :: i
    type(collision_struct) :: tmp_collision
    real :: tmp_time

    allocate(collision_list(size(particle_list)**2 - size(particle_list)))

    ! modifies collision_list
    call calculate_all_collisions(particle_list, collision_list)

    tmp_collision = collision_list(1)
    do i = 2, size(collision_list), 1
        tmp_time = collision_list(i)%collision_time
        if ( ieee_is_finite(tmp_time) ) then ! inf
            if (tmp_time .eq. tmp_time) then ! nan
                if (tmp_time < tmp_collision%collision_time) then
                    if (tmp_time > 0) then
                        tmp_collision = collision_list(i)
                    end if
                end if
            end if
        end if
    end do

    next_coll = tmp_collision
end function

! fast-forward every particle without the influence of gravity until the next collision
    ! time, which is supplied as an argument
subroutine fast_forward(particle_list, DT)
    type(particle), dimension(:), intent(inout) :: particle_list
    real, intent(in) :: dt

    ! local variables
    integer :: i

    ! subroutine
    do i = 1, size(particle_list), 1
        particle_list(i)%pos = particle_list(i)%pos + particle_list(i)%vel * dt
    end do

end subroutine fast_forward

! if any particles are overlapping / very close, calculate the collision between them
! MODIFIES particle_list
subroutine bbox_collisions(particle_list)
    type(particle), dimension(:), intent(inout) :: particle_list
    integer :: i,j
    real :: sep_buf
    logical, dimension(:,:), allocatable :: coll_list

    sep_buf = 0.05 * particle_list(1)%radius

    do i = 1, size(particle_list), 1
        do j = 1, size(particle_list), 1
            if (i .ne. j) then
                 if (norm2(particle_list(i)%pos - particle_list(j)%pos) <= &
                        particle_list(i)%radius + particle_list(j)%radius + sep_buf) then
                    call collide(particle_list(i), particle_list(j))
                 end if
            end if
        end do
    end do


end subroutine bbox_collisions

! calculate a new timestep s.t.: (v_max + v_second_max) * new_dt < 2*particle_radius
! ASSUMES uniform radii, but it's also just a heuristic so not a huge deal, could also precompute average radius size
real function calculate_next_dt(particle_list) result(new_dt)
    type(particle), dimension(:), intent(in) :: particle_list

    integer :: i
    real, dimension(:), allocatable :: v_list 
    real :: v_max, v_second_max

    allocate(v_list(size(particle_list)))

    do i = 1, size(particle_list), 1
        v_list(i) = norm2(particle_list(i)%vel)
    end do

    v_max = maxval(v_list)
    v_second_max = maxval(v_list, mask = v_list .le. v_max)

    new_dt = (2.0 * particle_list(1)%radius) / ((v_max + v_second_max))

end function

end module collision_module