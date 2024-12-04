module utility_module
    use stdlib_str2num, only: to_num
    use stdlib_ascii, only: is_digit
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


subroutine read_line(line, total)
    character(len=*), intent(in) :: line
    integer, intent(inout) :: total
    integer :: i, pos, ierr
    character(len=4000) :: temp

    ! Count the element
    pos = 1
    temp = trim(line)
    do
        pos = scan(temp(pos:), 'm') + pos
        if( pos > len(temp)) exit
        call get_mult( temp(pos-1:pos+12), total)
        if (scan(temp(pos:), 'm') == 0 .or. pos > len(temp)) exit
    end do

end subroutine read_line


subroutine read_line_with_do_dont(line, total, is_do)
    character(len=*), intent(in) :: line
    integer, intent(inout) :: total
    integer :: i, pos, ierr
    integer :: pos_do, pos_dont
    character(len=4000) :: temp
    logical, intent(inout) :: is_do 
    ! Count the element
    pos = 1
    temp = trim(line)
    do
        pos = scan(temp(pos:), 'md') + pos
        if( temp(pos-1:pos+2) == "do()") is_do = .true.
        if( temp(pos-1:pos+5) == "don't()") is_do = .false.
        if( is_do )call get_mult( temp(pos-1:pos+12), total)

        if (scan(temp(pos:), 'md') == 0 .or. pos > len(temp)) exit
    end do

end subroutine read_line_with_do_dont

subroutine get_mult(mult_part, total)
    character(len=13), intent(in) :: mult_part
    integer , intent(inout) :: total
    integer :: first_number, second_number
    integer :: pos_r_paren
    integer :: pos_comma
    integer :: i
    character(len=1) :: test_case

    if( mult_part(1:4) /= "mul(") return
    pos_r_paren = scan(mult_part(4:), ')') + 3
    if( pos_r_paren == 3) return
    ! After pos 5 because that pos need to be a number
    pos_comma = scan(mult_part(5:), ',', .false.) + 4
    if( pos_comma == 4 .or. abs(pos_comma - pos_r_paren) < 2) return
    ! Check if the two part of mult is number
    do i=5,pos_comma-1
        test_case = mult_part(i:i)
        if( .not. is_digit( test_case ) )return 
    enddo
    do i=pos_comma+1, pos_r_paren-1
        test_case = mult_part(i:i)
        if( .not. is_digit( test_case ) ) return 
    enddo

    ! Get the number
    first_number = to_num( mult_part(5:pos_comma-1) , first_number)
    second_number = to_num( mult_part(pos_comma+1: pos_r_paren-1), second_number)
    total = total + first_number * second_number
end subroutine get_mult


end module utility_module

program problem_2
    use utility_module
    implicit none
    
    character(len=*), parameter :: finput = "input.txt"
    integer :: N, i
    character(len=4000) :: line
    integer :: total = 0
    integer :: err, io
    integer :: reading_unit = 50
    ! Part 2
    integer :: total_do_dont = 0
    logical :: is_continue = .true.

    call lines_in_file( finput, N)
    open(unit=reading_unit, file=finput, status="old", iostat=err)
    if(err /= 0) then
        print *, "Error opening file"
        stop 
    endif
 
    do i=1,N
        read(reading_unit, '(A)' ) line
        call read_line(line, total)
        call read_line_with_do_dont(line, total_do_dont, is_continue)
    enddo
    close(reading_unit)
    
    print *, "mul total: ",total 
    print *, "mul do don't total", total_do_dont 

end program problem_2

