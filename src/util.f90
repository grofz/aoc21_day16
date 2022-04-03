  module utils
    implicit none

    integer, parameter :: LK = selected_int_kind(1)
    integer, parameter :: I8 = selected_int_kind(18)

    type bitstore_t
      logical(LK), allocatable :: store(:)
      integer :: ibeg = -1, iend = -1
    contains
      procedure :: load => bitstore_load
      procedure :: get => bitstore_get
      procedure :: canget => bitstore_canget
    end type



    type packet_t
      integer :: ver   ! version
      integer :: id    ! id
      integer :: ilab  ! 1 or 0  ! TODO make logical
      integer :: num   ! number of subpackets
      integer :: size  ! size in bits
      integer(I8) :: val
      !logical(LK), allocatable :: raw(:)
    end type

  contains

    subroutine buypacket(packet, store)
      type(packet_t), intent(out) :: packet
      type(bitstore_t), intent(inout) :: store

      logical(LK), allocatable :: tmp(:), half(:), literal(:)
      logical :: is_ok
      integer :: ibeg, nhalf

      ibeg = store % ibeg

      ! try to read packet's head
      if (store % canget(6)) then
         call store % get(3, tmp, is_ok)
         packet % ver = bits2int(tmp)
         call store % get(3, tmp, is_ok)     
         packet % id = bits2int(tmp)
      else
  print *, 'buypacket: nothing in store '
        error stop 
        return
      endif

      select case (packet % id) 
      case(4) ! literal value
        allocate(literal(0))
        nhalf = 0
        do
          call store % get(1, tmp, is_ok)
          call store % get(4, half, is_ok)
          literal = [literal, half]
          nhalf = nhalf + 1
          if (.not. tmp(1)) exit
        enddo
        if (nhalf > 16) error stop '8 byte integer overflow'
        packet % val = bits2int(literal)

      case default ! sub-packets
        call store % get(1, tmp, is_ok)
        if (tmp(1)) then 
          ! next 11 bits represent number of subpackets
          packet % ilab = 1
          call store % get(11, tmp, is_ok)
          packet % num = bits2int(tmp)

        else 
          ! next 15 bits represent total length of bits in subpackets
          packet % ilab = 0
          call store % get(15, tmp, is_ok)
          packet % num = bits2int(tmp)
        endif
      end select
      packet % size = store % ibeg - ibeg

      if (packet % id == 4) then
        print '(a,i1,a,i0,a,i0,a)', &
        'V', packet%ver, ' value = ', packet%val, &
        ' (', packet%size,'b)'
      else
        print '(a,i1,a,i1,a,i1,a,i0,a,i0,a)', &
        'V', packet%ver,' [', packet%id,']  ', packet%ilab,'=', packet%num, &
        ' (', packet%size,'b)'
      endif
    end subroutine



    logical function bitstore_canget(this, n)
      class(bitstore_t), intent(in) :: this
      integer, intent(in) :: n
      bitstore_canget = this % ibeg + n - 1 <= this % iend
    end function



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



    subroutine bitstore_get(this, n, bits, is_success)
      class(bitstore_t), intent(inout) :: this
      integer, intent(in) :: n
      logical(LK), allocatable, intent(out) :: bits(:)
      logical, intent(out) :: is_success
 !
 ! Get "n" bits from the store. Empty array given if requesting more bits
 ! than in is stored.
 !
      is_success = .true.
      if (this % ibeg + n - 1 > this % iend) is_success = .false.

      if (is_success) then
        allocate(bits(n))
        bits = this % store(this % ibeg : this % ibeg + n - 1)
        this % ibeg = this % ibeg + n
      else
        allocate(bits(0))
        return
      endif
    end subroutine



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

      int = 0
      do i = 1, size(bits)
        if (bits(i)) int = int + 2_I8**(size(bits)-i)
      enddo
    end function



  end module utils
