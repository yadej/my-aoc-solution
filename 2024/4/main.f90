module utility_module
    implicit none
contains 

subroutine lines_in_file(input, number_of_lines, size)
    character(len=*), intent(in) :: input
    integer, intent(out) :: number_of_lines
    integer, intent(out) :: size
    character(len=200 ) :: line
    integer :: io

    number_of_lines = 0
    size = 0
    open(unit=1, file=input, status="old", iostat=io)
    do
        read(1, '(A)', iostat=io) line
        size = max( len_trim(line), size)
        if(io /=0) exit
        number_of_lines = number_of_lines + 1
    enddo
    close(1) 
end subroutine lines_in_file


! Part 1 function
! Can Be more efficient if we combine all the for loop
! Can be more efficient if do the check if the first letter is X
subroutine get_XMAS( word_grid, number_of_lines, size_line, number_of_xmas)
    
    integer, intent(in) :: number_of_lines, size_line
    character(len=size_line), intent(in) :: word_grid(number_of_lines)
    integer, intent(out) :: number_of_xmas
    character(len=4) :: current_word
    integer :: i, j
    number_of_xmas = 0

    ! check XMAS line
    do i=1,number_of_lines
        do j=1,size_line-3
            current_word = word_grid(i)(j:j+3)
            if( current_word == "XMAS" .or. current_word == "SAMX") then
                number_of_xmas = number_of_xmas + 1
            endif
        enddo
    enddo
    ! check XMAS col
    do i=1,number_of_lines-3
        do j=1,size_line
            current_word=word_grid(i)(j:j)//word_grid(i+1)(j:j)//word_grid(i+2)(j:j)//word_grid(i+3)(j:j)
            if( current_word == "XMAS" .or. current_word == "SAMX") then
                number_of_xmas = number_of_xmas + 1
            endif
        enddo
    enddo

    ! check XMAS Diag right
    do i=1,number_of_lines-3
        do j=1,size_line-3
            current_word=word_grid(i)(j:j)//word_grid(i+1)(j+1:j+1)//word_grid(i+2)(j+2:j+2)//word_grid(i+3)(j+3:j+3)
            if( current_word == "XMAS" .or. current_word == "SAMX") then
                number_of_xmas = number_of_xmas + 1
            endif
        enddo
    enddo

    ! check XMAS Diag left
    do i=4,number_of_lines
        do j=4,size_line
            current_word=word_grid(i-3)(j:j)//word_grid(i-2)(j-1:j-1)//word_grid(i-1)(j-2:j-2)//word_grid(i)(j-3:j-3)
            if( current_word == "XMAS" .or. current_word == "SAMX") then
                number_of_xmas = number_of_xmas + 1
            endif
        enddo
    enddo

end subroutine get_XMAS


function is_X_MAS(input) result(output)
    character(len=4), intent(in) :: input
    logical :: output

    output = input == "SSMM" .or. input == "SMSM" .or. input == "MMSS" .or. input == "MSMS"
                                
end function is_X_MAS

! Part 2 function
subroutine get_X_MAS( word_grid, number_of_lines, size_line, number_of_x_mas)
    integer, intent(in) :: number_of_lines, size_line
    character(len=size_line), intent(in) :: word_grid(number_of_lines)
    integer, intent(out) :: number_of_x_mas
    character(len=4) :: current_word
    integer :: i, j
    number_of_x_mas = 0

    do i=2, number_of_lines-1
        do j=2, size_line-1
            if( word_grid(i)(j:j) == "A") then
                current_word = word_grid(i-1)(j-1:j-1)//word_grid(i+1)(j-1:j-1) &
                    // word_grid(i-1)(j+1:j+1)//word_grid(i+1)(j+1:j+1)
                if( is_X_MAS(current_word) ) number_of_x_mas = number_of_x_mas + 1
            endif
        enddo
    enddo
end subroutine get_X_MAS

end module utility_module

program problem_4
    use utility_module
    implicit none
    
    character(len=*), parameter :: finput = "example.txt"
    integer :: N, i, size_line
    character(:), allocatable:: line(:)
    integer :: number_of_element
    integer :: total = 0
    integer :: err, io
    integer :: reading_unit = 50
    ! Part 2
    integer :: total_x_max
    call lines_in_file( finput, N, size_line)
    open(unit=reading_unit, file=finput, status="old", iostat=err)
    if(err /= 0) then
        print *, "Error opening file"
        stop 
    endif

    allocate(character(size_line) :: line(N) , stat=err)
    if (err /= 0) print *, "line(N) : Allocation request denied"
    do i=1,N
        read(reading_unit, "(A)") line(i)
    enddo

    close(reading_unit)
    call get_XMAS(line, N, size_line, total)
    call get_X_MAS(line, N, size_line, total_x_max) 
    print *, "total number of xmas:", total
    print *, "total number of x-mas", total_x_max

    if (allocated(line)) deallocate(line, stat=err)
    if (err /= 0) print *, "line(N) : Deallocation request denied"

end program problem_4

