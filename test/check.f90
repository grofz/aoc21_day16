  program check
    use utils
    implicit none

    character(len=:), allocatable ::  input_file
    integer :: i, j
    integer(I8) :: val
    type(bitstore_t) :: bitstore
    type(packet_t)   :: one_packet
    type(packet_t), allocatable :: packets(:)

    do 
      write(*,'(a)',advance='no') 'AoC decode message. Which data (0-7) ? '
      read(*,*) i
      select case(i)
      case(0)
        input_file = 'input.txt'
      case(1)
        input_file = 'test1.txt'
      case(2)
        input_file = 'test2.txt'
      case(3)
        input_file = 'test3.txt'
      case(4)
        input_file = 'test4.txt'
      case(5)
        input_file = 'test5.txt'
      case(6)
        input_file = 'test6.txt'
      case(7)
        input_file = 'test.txt'
      case default
        cycle
      end select
      exit
    enddo

    call bitstore % load(input_file)
    allocate(packets(0))

    i = 0
    do

      call buypacket(one_packet, bitstore)
      packets = [packets, one_packet]
      print *

      i = i + one_packet % ver

      if (bitstore % ibeg + 7 > bitstore % iend) exit

    enddo
    print *, i
    print *, 'remain in store = ', bitstore%iend - bitstore%ibeg+1
    print *, 'number of unique packets ', size(packets)

    call findsubpackets(1, packets, i, j)
    call evalsubpackets(1, packets, val)
    call printsubpackets(packets)
    print *, 'Value of 1 =', val

end program check
