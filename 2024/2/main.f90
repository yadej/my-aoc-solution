module utility_module
    implicit none
contains 

subroutine lines_in_file(input, number_of_lines)
    character(len=*), intent(in) :: input
    integer, intent(out) :: number_of_lines
    integer :: io

    number_of_lines = 0
    open(unit=1, file=input, status="old", iostat=io)
    do
        read(1, *, iostat=io)
        if(io /=0) exit
        number_of_lines = number_of_lines + 1
    enddo
    close(1) 
end subroutine lines_in_file


subroutine read_line(line, array, num_elements)
    character(len=*), intent(in) :: line
    integer, allocatable, intent(out) :: array(:)
    integer, intent(out) :: num_elements
    integer :: i, pos, ierr
    character(len=100) :: temp
    integer :: value

    ! Count the element
    num_elements = 0
    pos = 1
    temp = trim(line)

    do
        read(temp(pos:), *, iostat=ierr) value
        if (ierr /= 0) exit
        num_elements = num_elements + 1
        pos = scan(temp(pos:), ' ') + pos
        if (scan(temp(pos:), ' ') == 0 .or. pos > len_trim(temp)) exit
    end do

    ! Allocate the array
    allocate(array(num_elements))
    ! Fill the array
    pos = 1
    do i = 1, num_elements
        read(temp(pos:), *, iostat=ierr) array(i)
        pos = scan(temp(pos:), ' ')+ pos
    end do
end subroutine read_line

subroutine is_safe(array, num_elements, total_sum)
    integer, intent(in) :: array(num_elements)
    integer, intent(in) :: num_elements
    integer, intent(inout) :: total_sum
    integer :: i, current_number, next_number, safe_count
    if( num_elements == 0) return
    safe_count = 0
    current_number = array(1)
    next_number = array(2)
    if( current_number < next_number) then
        if( abs(next_number - current_number) > 3)return
        do i=3,num_elements
            current_number = next_number
            next_number = array(i)
            if( current_number >= next_number .or. (next_number - current_number) > 3)return
        enddo
        total_sum = total_sum  + 1
    else
        if( abs(next_number - current_number) > 3 .or. next_number == current_number)return
        do i=3,num_elements
            current_number = next_number
            next_number = array(i)
            if( current_number <= next_number .or. abs(current_number - next_number) > 3)return
        enddo
        total_sum = total_sum  + 1
    endif
end subroutine is_safe


recursive subroutine is_safe_damp(array, num_elements, total_sum, safe_count)
    integer, intent(in) :: array(num_elements)
    integer, intent(in) :: num_elements
    integer, intent(inout) :: safe_count
    integer, intent(inout) :: total_sum
    integer :: copy_safe_count
    integer :: copy_total_sum
    integer :: i, current_number, next_number

    if( num_elements == 0) return
    if( safe_count >= 2) return
    current_number = array(1)
    next_number = array(2)
    if( current_number < next_number) then
        if( abs(next_number - current_number) > 3)then
            safe_count = safe_count + 1
            copy_safe_count = safe_count
            call is_safe_damp( array(2:), num_elements - 1, total_sum, safe_count) 
            call is_safe_damp( [array(1:1), array(3:) ],num_elements-1, total_sum, copy_safe_count)
            return
        endif
        do i=3,num_elements
            current_number = next_number
            next_number = array(i)
            if( current_number >= next_number .or. abs(next_number - current_number) > 3) then 
                safe_count = safe_count + 1
                next_number = current_number
            endif
            if( safe_count >= 2 )return
        enddo
    else
        ! The test is done if we remove the second but never tested if it worked of the first
        if( abs(next_number - current_number) > 3 .or. next_number == current_number) then 
            safe_count = safe_count + 1
            copy_safe_count = safe_count
            call is_safe_damp( array(2:), num_elements - 1, total_sum, safe_count) 
            call is_safe_damp( [array(1:1), array(3:)], num_elements-1, total_sum, copy_safe_count)
            return
            endif
        do i=3,num_elements
            current_number = next_number
            next_number = array(i)
            if( current_number <= next_number .or. abs(current_number - next_number) > 3) then 
                safe_count = safe_count + 1
                next_number = current_number
            endif
            if( safe_count >= 2)return
        enddo
    endif
    total_sum = total_sum  + 1

end subroutine is_safe_damp

end module utility_module

program problem_2
    use utility_module
    implicit none
    
    character(len=*), parameter :: finput = "input.txt"
    integer :: N, i, j
    character(len=50) :: line
    integer :: number_of_element
    integer :: total = 0
    integer :: err, io
    integer :: reading_unit = 50
    integer, allocatable :: int_list(:)
    !For the second Part
    integer :: total_damp = 0
    integer :: for_the_inout = 0

    call lines_in_file( finput, N)
    open(unit=reading_unit, file=finput, status="old", iostat=err)
    if(err /= 0) then
        print *, "Error opening file"
        stop 
    endif
 
    do i=1,N
        read(reading_unit, '(A)' ) line
        call read_line(line, int_list , number_of_element)
        call is_safe(int_list, number_of_element, total)
        call is_safe_damp(int_list, number_of_element, total_damp, for_the_inout)
        for_the_inout = 0
    enddo
    close(reading_unit)

    print *, "number of safe code is: ",total
    print *, "number of safe code with damp is", total_damp

end program problem_2

