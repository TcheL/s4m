!******************************************************************************!
!*  This is a program that compute the mean, median, minimum and maximum of   *!
!*       a set of data.                                                       *!
!*                                                                            *!
!*  Author: Tche LIU          Email: seistche@gmail.com                       *!
!*  Copyright (C) Tche, 2018. All Rights Reserved.                            *!
!******************************************************************************!

!===============================================================================

#define DOUBLETYPE
#define LLS 128
#define LSS 20
#define MAX_WIDTH_STDIN (LLS * 10000)

!===============================================================================

module Para_mod

  implicit none
  public

#ifndef DOUBLETYPE
  integer, parameter :: MK = 4
#else
  integer, parameter :: MK = 8
#endif

  character(len = :), allocatable :: ErrorPath
  character(len = :), allocatable :: ErrorInfo

  character(len = :), allocatable :: FileName
  integer :: SetSize
  real(kind = MK), allocatable :: Dat(:)
  logical :: IsFileGet = .false.
  logical :: IsStdinGet = .false.

  contains

    subroutine Para_Init()
      SetSize = 0
      FileName = 'NULL'
      ErrorPath = '/s4m'
      ErrorInfo = 'NULL'
    end subroutine Para_Init

    subroutine Para_FileGetData()
      integer :: fid, ios
      logical :: IsExist
      ErrorPath = ErrorPath//'/Para_FileGetData'
      ErrorInfo = 'Except when read data from the file: '//FileName
      inquire(file = FileName, exist = IsExist)
      if(.not. IsExist) call Para_ErrorExcept(.true., 'File not found')
      open(newunit = fid, file = FileName)
        read(fid, *, iostat = ios) Dat
        if(ios < 0) call Para_ErrorExcept(.true., 'Insufficient data')
        if(ios > 0) call Para_ErrorExcept(.true., 'Improper data type')
      close(fid)
      ErrorPath = ErrorPath(:index(ErrorPath, '/', .true.) - 1)
    end subroutine Para_FileGetData

    subroutine Para_StdinGetData()
      character(len = :), allocatable :: DatString
      character(len = MAX_WIDTH_STDIN) :: instr, fmtstr
      integer :: ios, k
      ErrorPath = ErrorPath//'/Para_StdinGetData'
      ErrorInfo = 'Except when read data from stdin'
      DatString = ''
      write(fmtstr, '(A, G0, A)') '(A', MAX_WIDTH_STDIN, ')'
      write(*, '(A)') '>> Please input the data set, and press <Ctrl-D> to end:'
      do while(.true.)
        read(*, fmtstr, iostat = ios) instr
        if(ios < 0) exit
        k = 1
        do while(.true.)
          instr = adjustl(instr(k:))
          if(len(trim(instr)) == 0) exit
          k = index(instr, ' ')
          DatString = DatString//instr(:k)
          SetSize = SetSize + 1
        end do
      end do
      allocate(Dat(SetSize))
      read(DatString, *, iostat = ios) Dat
      if(ios > 0) call Para_ErrorExcept(.true., 'Improper data type')
      ErrorPath = ErrorPath(:index(ErrorPath, '/', .true.) - 1)
    end subroutine Para_StdinGetData

    subroutine Para_Destroy()
      if(allocated(ErrorPath)) deallocate(ErrorPath)
      if(allocated(ErrorInfo)) deallocate(ErrorInfo)
      if(allocated(FileName)) deallocate(FileName)
      if(allocated(Dat)) deallocate(Dat)
    end subroutine Para_Destroy

    subroutine Para_ErrorExcept(IsError, SupplyString)
      logical, intent(in) :: IsError
      character(len = *), intent(in), optional :: SupplyString
      if(IsError) then
        write(*, *)
        write(*, '(A)') 'ERROR:'
      else
        write(*, *)
        write(*, '(A)') 'WARNING:'
      end if
      write(*, '(A)') '  Error Path: '//ErrorPath
      write(*, '(A)') '  Error Info: '//ErrorInfo
      if(present(SupplyString)) write(*, '(A)') '  '//SupplyString
      write(*, *)
      if(IsError) stop 2
    end subroutine Para_ErrorExcept

end module Para_mod

!===============================================================================

module CmdP_mod

  use Para_mod, only : MK, SetSize, FileName, Dat, IsFileGet, IsStdinGet, &
    & ErrorPath, ErrorInfo, Para_ErrorExcept
  implicit none
  private

  interface CmdP_ChangePara
    module procedure CmdP_ChangePara_Int
    module procedure CmdP_ChangePara_Real
    module procedure CmdP_ChangePara_Char
  end interface CmdP_ChangePara

  public :: CmdP_GetProcess, CmdP_PrintHelpInfo

  contains

    subroutine CmdP_GetProcess()
      character(len = LLS) :: CmdOpt
      character(len = LSS) :: IndexStr
      integer :: nArg, i
      ErrorPath = ErrorPath//'/CmdP_GetProcess'
      nArg = command_argument_count()
      if(nArg == 0) then
        IsStdinGet = .true.
      else if(nArg == 1) then
        call get_command_argument(1, CmdOpt)
        if(trim(adjustl(CmdOpt)) == '-h' .or. &
          & trim(adjustl(CmdOpt)) == '--help') then
          call CmdP_PrintHelpInfo()
          stop 0
        else
          ErrorInfo = 'Except when handling the #1 command line argument.'
          call CmdP_PrintHelpInfo()
          call Para_ErrorExcept(.true., 'Illegal command line argument: '// &
            & trim(adjustl(CmdOpt)))
        end if
      else
        IsStdinGet = .false.
        call get_command_argument(1, CmdOpt)
        if(trim(adjustl(CmdOpt)) == '-f' .or. &
          & trim(adjustl(CmdOpt)) == '--file') then
          IsFileGet = .true.
          ErrorInfo = 'Except when handling the #2 command line argument.'
          call CmdP_ChangePara(SetSize, 2, 'SetSize')
          allocate(Dat(SetSize))
          call CmdP_ChangePara(FileName, 3, 'FileName')
        else
          IsFileGet = .false.
          SetSize = nArg
          allocate(Dat(SetSize))
          do i = 1, nArg
            write(IndexStr, '(G0)') i
            ErrorInfo = 'Except when handling the #'//trim(adjustl(IndexStr)) &
              & //' command line argument.'
            call CmdP_ChangePara(Dat(i), i, 'DataPoint' &
              & //trim(adjustl(IndexStr)))
          end do
        end if
      end if
      ErrorPath = ErrorPath(:index(ErrorPath, '/', .true.) - 1)
    end subroutine CmdP_GetProcess

    subroutine CmdP_ChangePara_Int(Var, ith, String)
      integer, intent(out) :: Var
      integer, intent(in) :: ith
      character(len = *), intent(in) :: String
      character(len = LLS) :: CmdStr
      integer :: ios
      ErrorPath = ErrorPath//'/CmdP_ChangePara_Int'
      call get_command_argument(ith, CmdStr)
      read(CmdStr, *, iostat = ios) Var
      if(ios > 0) then
        call CmdP_PrintHelpInfo()
        call Para_ErrorExcept(.true., String//' should be a INTEGER number.')
      end if
      ErrorPath = ErrorPath(:index(ErrorPath, '/', .true.) - 1)
    end subroutine CmdP_ChangePara_Int
    subroutine CmdP_ChangePara_Real(Var, ith, String)
      real(kind = MK), intent(out) :: Var
      integer, intent(in) :: ith
      character(len = *), intent(in) :: String
      character(len = LLS) :: CmdStr
      integer :: ios
      ErrorPath = ErrorPath//'/CmdP_ChangePara_Real'
      call get_command_argument(ith, CmdStr)
      read(CmdStr, *, iostat = ios) Var
      if(ios > 0) &
        & call Para_ErrorExcept(.true., String//' should be a REAL number.')
      ErrorPath = ErrorPath(:index(ErrorPath, '/', .true.) - 1)
    end subroutine CmdP_ChangePara_Real
    subroutine CmdP_ChangePara_Char(Var, ith, String)
      character(len = :), allocatable, intent(out) :: Var
      integer, intent(in) :: ith
      character(len = *), intent(in) :: String
      character(len = LLS) :: CmdStr
      call get_command_argument(ith, CmdStr)
      Var = trim(adjustl(CmdStr))
    end subroutine CmdP_ChangePara_Char

    subroutine CmdP_PrintHelpInfo()
      write(*, *)
      write(*, '(A)') 'Usage:'
      write(*, '(A)') '  s4m'
      write(*, '(A)') '    read data set from stdin.'
      write(*, '(A)') '  s4m DataPoint1 DataPoint2 [ DataPoint3 ... ]'
      write(*, '(A)') '    read data set from command line arguments.'
      write(*, '(A)') '  s4m -f SetSize FileName'
      write(*, '(A)') '    read data set from the specific input file.'
      write(*, *)
    end subroutine CmdP_PrintHelpInfo

end module CmdP_mod

!===============================================================================

module Stat_mod
  
  use Para_mod, only : MK
  implicit none
  private

  public :: Stat_Mean, Stat_Median

  contains

    function Stat_Mean(dat, n) result(mean)
      integer, intent(in) :: n
      real(MK), intent(in) :: dat(n)
      real(MK) :: mean
      mean = sum(dat)/n
    end function Stat_Mean

    function Stat_Median(dat, n) result(median)
      integer, intent(in) :: n
      real(MK), intent(in) :: dat(n)
      real(MK) :: datcopy(n), median
      datcopy = dat
      median = median_left(datcopy, 1, n, n)
      if(mod(n, 2) == 0) then
        datcopy = dat
        median = (median + median_right(datcopy, 1, n, n))/2.0_MK
      end if
    end function Stat_Median

    recursive function median_left(dat, left, right, n) result(median)
      real(MK) :: dat(:)
      integer, intent(in) :: left, right, n
      real(MK) :: median, d
      integer :: ll, rr
      ll = left; rr = right
      d = dat(left)
      do while(ll < rr)
        do while(ll < rr .and. dat(rr) >= d)
          rr = rr - 1
        end do
        dat(ll) = dat(rr)
        do while(ll < rr .and. dat(ll) <= d)
          ll = ll + 1
        end do
        dat(rr) = dat(ll)
      end do
      if(ll > (n + 1)/2) then
        median = median_left(dat, left, ll - 1, n)
      else if(ll == (n + 1)/2) then
        median = d
      else
        median = median_left(dat, ll + 1, right, n)
      end if
    end function median_left
    recursive function median_right(dat, left, right, n) result(median)
      real(MK) :: dat(:)
      integer, intent(in) :: left, right, n
      real(MK) :: median, d
      integer :: ll, rr
      ll = left; rr = right
      d = dat(left)
      do while(ll < rr)
        do while(ll < rr .and. dat(rr) >= d)
          rr = rr - 1
        end do
        dat(ll) = dat(rr)
        do while(ll < rr .and. dat(ll) <= d)
          ll = ll + 1
        end do
        dat(rr) = dat(ll)
      end do
      if(ll > (n + 2)/2) then
        median = median_right(dat, left, ll - 1, n)
      else if(ll == (n + 2)/2) then
        median = d
      else
        median = median_right(dat, ll + 1, right, n)
      end if
    end function median_right

end module Stat_mod

!===============================================================================

program main

  use Para_mod
  use CmdP_mod
  use Stat_mod
  implicit none

  call Para_Init()
  call CmdP_GetProcess()

  if(IsFileGet) call Para_FileGetData()
  if(IsStdinGet) call Para_StdinGetData()

  write(*, *)
  write(*, '(A, G0)') 'Mean    = ', Stat_Mean(Dat, SetSize)
  write(*, '(A, G0)') 'Median  = ', Stat_Median(Dat, SetSize)
  write(*, '(A, G0)') 'Minimum = ', minval(Dat)
  write(*, '(A, G0)') 'Maximum = ', maxval(Dat)
  write(*, *)

  call Para_Destroy()

end program main
