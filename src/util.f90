  module utils
    implicit none
    integer, parameter :: LK = selected_int_kind(1)
    integer, parameter :: I8 = selected_int_kind(18)
    integer, parameter :: MAX_BIT_SIZE = 64 ! I8 is 64bit

    type bitstore_t
      logical(LK), allocatable :: store(:)
      integer :: ibeg = -1, iend = -1
    contains
      procedure :: load => bitstore_load
      procedure :: get => bitstore_get
      procedure :: canget => bitstore_canget
    end type



    type packet_t
      integer :: ver      ! version
      integer :: id       ! id (4=value else=operator)
      integer(I8) :: val  ! value
      logical(LK) :: isn  ! .true. -> num represents no. of subpackets
      integer :: num      ! no. of sub-packets / bits in sub-packets
      integer :: size     ! size in bits
      integer, allocatable :: mem(:) ! list of sub-packets
    end type

  contains
    recursive subroutine evalsubpackets(myid, arr, val)
      type(packet_t), intent(inout) :: arr(:)
      integer, intent(in) :: myid
      integer(I8), intent(out) :: val

      integer(i8) :: val1, val2
      integer :: i

      ! If packet is "literal value", just return the value
      if (arr(myid) % id == 4) then
        val = arr(myid) % val
        return
      endif

      ! If operator is ">", "<", "=="
      if (arr(myid) % id > 4) then
        if (size(arr(myid) % mem) /= 2) &
            error stop 'relational operator does not have two sub-packets'
        call evalsubpackets(arr(myid) % mem(1), arr,  val1)
        call evalsubpackets(arr(myid) % mem(2), arr, val2)
        val = 0
        select case(arr(myid) % id)
        case(5) ! ">"
          if (val1 > val2) val = 1
        case(6) ! "<'
          if (val1 < val2) val = 1
        case(7) ! "=="
          if (val1 == val2) val = 1
        case default
          error stop 'invalid id'
        end select
        arr(myid) % val = val
        return
      endif

      ! Operator is sum, prod, min, max
      select case(arr(myid)%id)
      case(0) ! sum
        val = 0
      case(1) ! product
        val = 1
      case(2) ! min
        val = huge(val)
      case(3) ! max
        val = -huge(val)
      case default
      end select
      do i = 1, size(arr(myid) % mem)
        call evalsubpackets(arr(myid) % mem(i), arr, val1)
        select case(arr(myid)%id)
        case(0) ! sum
          val = val + val1
        case(1) ! product
          val = val * val1
        case(2) ! min
          val = min(val, val1)
        case(3) ! max
          val = max(val, val1)
        end select
      enddo
      arr(myid) % val = val
    end subroutine evalsubpackets



    recursive subroutine findsubpackets(myid, arr, lastid, nbits)
      integer, intent(in) :: myid
      type(packet_t), intent(inout) :: arr(:)
      integer, intent(out) :: lastid, nbits

      integer :: nbits0, nsubpackets0, lastid1, nbits1
      if (myid > size(arr)) &
          error stop 'out of array bounds'

      allocate(arr(myid) % mem(0))
      lastid = myid
      nbits = arr(myid) % size
      if (arr(myid) % id == 4) return

      ! packet is an operator
      nbits0 = 0
      nsubpackets0 = 0
      do
        ! continue unless all required subpackets received
        if (arr(myid) % isn) then
          if (nsubpackets0 == arr(myid) % num) exit
          if (nsubpackets0 > arr(myid) % num) &
              error stop 'find subpackets error'
        
        else
          if (nbits0 == arr(myid) % num) exit
          if (nbits0 > arr(myid) % num) &
              error stop 'find subpackets error - overflow'
        
        endif

        arr(myid) % mem = [arr(myid) % mem, lastid+1]
        call findsubpackets(lastid+1, arr, lastid1, nbits1)
        lastid = lastid1
        nbits0 = nbits0 + nbits1
        nsubpackets0 = nsubpackets0 + 1
      enddo
      nbits = nbits + nbits0
    end subroutine findsubpackets



    subroutine printsubpackets(arr)
      type(packet_t), intent(in) :: arr(:)
      integer :: i

      do i = 1, size(arr)
      if (arr(i) % id == 4) then
        write(*, '(i4,a,i1,a,i0,a,i0,a)', advance='no') i, &
        ' V', arr(i)%ver, ' value = ', arr(i)%val, &
        ' (', arr(i)%size,'b)    '
      else
        write(*, '(i4,a,i1,a,i1,a,l1,a,i0,a,i0,a,i0,a)', advance='no') i, &
        ' V', arr(i)%ver,' [', arr(i)%id,']  ', arr(i)%isn,'=', arr(i)%num, &
        ' (', arr(i)%size,'b)   value = {', arr(i)%val,'}   '
      endif
      if (size(arr(i) % mem) > 0 .and. allocated(arr(i)%mem)) then
        write(*,'(20(i0,1x))') arr(i) % mem
      else
        write(*,'(a)') 'no subpackets'
      endif
      enddo
    end subroutine printsubpackets



    subroutine buypacket(packet, store)
      type(packet_t), intent(out) :: packet
      type(bitstore_t), intent(inout) :: store

      logical(LK), allocatable :: tmp(:), half(:), literal(:)
      integer :: ibeg, nhalf

      ibeg = store % ibeg

      ! try to read packet's head
      if (store % canget(6)) then
         call store % get(3, tmp)
         packet % ver = bits2int(tmp)
         call store % get(3, tmp)     
         packet % id = bits2int(tmp)
      else
  print *, 'buypacket: nothing in store '
        error stop 
        return
      endif

      select case (packet % id) 
      case(4) ! packet is "literal value"
        allocate(literal(0))
        nhalf = 0
        do
          call store % get(1, tmp)
          call store % get(4, half)
          literal = [literal, half]
          nhalf = nhalf + 1
          if (.not. tmp(1)) exit
        enddo
        packet % val = bits2int(literal)
        packet % num = -1

      case default ! packet is "operator"
        call store % get(1, tmp)
        if (tmp(1)) then 
          ! next 11 bits represent number of subpackets
          packet % isn = .true.
          call store % get(11, tmp)
        else 
          ! next 15 bits represent total length of bits in subpackets
          packet % isn = .false.
          call store % get(15, tmp)
        endif
        packet % val = -1
        packet % num = bits2int(tmp)
      end select
      packet % size = store % ibeg - ibeg

    end subroutine



    subroutine bitstore_load(this, file)
      use, intrinsic :: iso_fortran_env, only : IOSTAT_END, IOSTAT_EOR
      class(bitstore_t), intent(out) :: this
      character(len=*), intent(in) :: file
      character(len=1) :: ch
!
! Load store by bits from the file
!
      integer :: fid, ios, imax
      logical(LK), allocatable :: tmp(:)
      integer, parameter :: ONECHAR=4

      imax = 10
      allocate(this % store(imax))
      this % iend = 0 ! marks the number of bits in the store

      open(newunit=fid, file=file, status='old')
      do
        read(fid, '(a1)', advance='no', iostat=ios) ch
        if (ios == IOSTAT_EOR) then
          print *, 'reading next line'
          cycle
        endif
        if (ios == IOSTAT_END) exit
        if (ios /= 0) error stop 'uknown i/o error'
 write(*,'(a1)',advance='no') ch

        ! character read -> store it
        if (this % iend + ONECHAR > imax) then
          ! allocate new space
          imax = imax * 2
          allocate(tmp(imax))
          tmp(1 : this%iend) = this % store(1 : this%iend)
          call move_alloc(tmp, this % store)
        endif
        this % store(this%iend + 1 : this%iend + ONECHAR) = char2bits(ch)
        this % iend = this % iend + ONECHAR
      enddo
      close(fid)

      this % ibeg = 1 ! marks the first unread bit
  print *, 'Bits read = ',this % iend
    end subroutine



    subroutine bitstore_get(this, n, bits)
      class(bitstore_t), intent(inout) :: this
      integer, intent(in) :: n
      logical(LK), allocatable, intent(out) :: bits(:)
 !
 ! Get "n" bits from the store. "canget" can be used to verify that
 ! bits are in store.
 !
      if (this % ibeg + n - 1 > this % iend) &
          error stop 'bitstore_get: requested more than available'
      allocate(bits(n))
      bits = this % store(this % ibeg : this % ibeg + n - 1)
      this % ibeg = this % ibeg + n
    end subroutine bitstore_get



    logical function bitstore_canget(this, n)
      class(bitstore_t), intent(in) :: this
      integer, intent(in) :: n
      bitstore_canget = this % ibeg + n - 1 <= this % iend
    end function



    function char2bits(ch) result(bits)
      character(len=1), intent(in) :: ch
      logical(LK) :: bits(4)

      integer :: ich, i, arr(4)

      ich = iachar(ch)

      if (ich >= iachar('0') .and. ich <= iachar('9')) then
        ich = ich - iachar('0')  
      else if (ich >= iachar('A') .and. ich <= iachar('F')) then
        ich = ich - iachar('A') + 10   
      else
        error stop 'char2bits: invalid char'
      endif

      do i=4,1,-1
        arr(5-i) = ich / (2**(i-1))
        ich = ich - arr(5-i) * (2**(i-1)) 
      enddo
      bits = .false.
      where (arr==1) bits = .true.
    end function



    function bits2int(bits) result(int)
      logical(LK), intent(in) :: bits(:)
      integer(I8) :: int

      integer :: i

      if (size(bits) > MAX_BIT_SIZE) &
          error stop 'bits2int: integer may overflow'

      int = 0
      do i = 1, size(bits)
        if (bits(i)) int = int + 2_I8**(size(bits)-i)
      enddo
    end function

  end module utils
