program problem_1
    use stdlib_sorting, only: sort
    implicit none
    
    character(len=*), parameter :: finput = "input.txt"
    integer :: N, i
    integer, allocatable :: int_list(:,:)
    integer :: sum = 0
    integer :: err

    ! For the part 2
    ! I will do it like a left right pointer
    ! Since I already sort the 2 list
    integer :: left = 1, right = 1
    integer :: current_number = 0
    integer :: similarity_sum = 0

    call lines_in_file( finput, N)
    allocate(int_list(N, 2) , stat=err)
    if (err /= 0) print *, "int_list(N, 2) : Allocation request denied"
    open(unit=1, file=finput, status="old", iostat=err)
    if(err /= 0) then
        print *, "Error opening file"
        stop 
    endif
 
    do i=1,N
        read(1,*) int_list(i, 1), int_list(i, 2)
    enddo
    close(1)
    call sort(int_list(:,1))
    call sort(int_list(:,2))

    do i=1,N
        sum = sum + abs(int_list(i,1) - int_list(i,2))
    enddo

    print *, "Sum: ", sum
    do i=1,N
        if( current_number /= int_list(i, 1) ) then
            current_number = int_list(i, 1) 
            ! We put the pointer left in the right
            left = right
            ! We position the left pointer to the first of current_number
            do while(left < N .and. current_number > int_list(left, 2))
                left = left + 1 
            end do
            
            ! We position right at the end of the current_number
            right = left
            do while(right < N .and. current_number >= int_list(right,2))
                right = right + 1
            end do
        endif
        similarity_sum = similarity_sum + current_number * ( right - left )
    enddo
    
    print *,"Similarity Sum: ", similarity_sum


    

    if (allocated(int_list )) deallocate(int_list , stat=err)
    if (err /= 0) print *, "int_list(N, 2) : Deallocation request denied"


    

end program problem_1

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

