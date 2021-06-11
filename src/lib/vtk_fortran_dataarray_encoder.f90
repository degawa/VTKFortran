!| DataArray encoder, codecs: "ascii", "base64".
module vtk_fortran_dataarray_encoder
    use, intrinsic :: iso_fortran_env
    use befor64
    use penf, only: str, BYI1P, BYI2P, BYI4P, BYI8P, &
                    BYR4P, BYR8P, BYR16P

    implicit none
    private
    public :: encode_ascii_dataarray
    public :: encode_binary_dataarray

    interface encode_ascii_dataarray
        !! Ascii DataArray encoder.
        module procedure encode_ascii_dataarray1_rank1_real64, &
            encode_ascii_dataarray1_rank1_real32, &
            encode_ascii_dataarray1_rank1_int64, &
            encode_ascii_dataarray1_rank1_int32, &
            encode_ascii_dataarray1_rank1_int16, &
            encode_ascii_dataarray1_rank1_int8, &
            encode_ascii_dataarray1_rank2_real64, &
            encode_ascii_dataarray1_rank2_real32, &
            encode_ascii_dataarray1_rank2_int64, &
            encode_ascii_dataarray1_rank2_int32, &
            encode_ascii_dataarray1_rank2_int16, &
            encode_ascii_dataarray1_rank2_int8, &
            encode_ascii_dataarray1_rank3_real64, &
            encode_ascii_dataarray1_rank3_real32, &
            encode_ascii_dataarray1_rank3_int64, &
            encode_ascii_dataarray1_rank3_int32, &
            encode_ascii_dataarray1_rank3_int16, &
            encode_ascii_dataarray1_rank3_int8, &
            encode_ascii_dataarray1_rank4_real64, &
            encode_ascii_dataarray1_rank4_real32, &
            encode_ascii_dataarray1_rank4_int64, &
            encode_ascii_dataarray1_rank4_int32, &
            encode_ascii_dataarray1_rank4_int16, &
            encode_ascii_dataarray1_rank4_int8, &
            encode_ascii_dataarray3_rank1_real64, &
            encode_ascii_dataarray3_rank1_real32, &
            encode_ascii_dataarray3_rank1_int64, &
            encode_ascii_dataarray3_rank1_int32, &
            encode_ascii_dataarray3_rank1_int16, &
            encode_ascii_dataarray3_rank1_int8, &
            encode_ascii_dataarray3_rank3_real64, &
            encode_ascii_dataarray3_rank3_real32, &
            encode_ascii_dataarray3_rank3_int64, &
            encode_ascii_dataarray3_rank3_int32, &
            encode_ascii_dataarray3_rank3_int16, &
            encode_ascii_dataarray3_rank3_int8
    end interface encode_ascii_dataarray
    interface encode_binary_dataarray
        !! Binary (base64) DataArray encoder.
        module procedure encode_binary_dataarray1_rank1_real64, &
            encode_binary_dataarray1_rank1_real32, &
            encode_binary_dataarray1_rank1_int64, &
            encode_binary_dataarray1_rank1_int32, &
            encode_binary_dataarray1_rank1_int16, &
            encode_binary_dataarray1_rank1_int8, &
            encode_binary_dataarray1_rank2_real64, &
            encode_binary_dataarray1_rank2_real32, &
            encode_binary_dataarray1_rank2_int64, &
            encode_binary_dataarray1_rank2_int32, &
            encode_binary_dataarray1_rank2_int16, &
            encode_binary_dataarray1_rank2_int8, &
            encode_binary_dataarray1_rank3_real64, &
            encode_binary_dataarray1_rank3_real32, &
            encode_binary_dataarray1_rank3_int64, &
            encode_binary_dataarray1_rank3_int32, &
            encode_binary_dataarray1_rank3_int16, &
            encode_binary_dataarray1_rank3_int8, &
            encode_binary_dataarray1_rank4_real64, &
            encode_binary_dataarray1_rank4_real32, &
            encode_binary_dataarray1_rank4_int64, &
            encode_binary_dataarray1_rank4_int32, &
            encode_binary_dataarray1_rank4_int16, &
            encode_binary_dataarray1_rank4_int8, &
            encode_binary_dataarray3_rank1_real64, &
            encode_binary_dataarray3_rank1_real32, &
            encode_binary_dataarray3_rank1_int64, &
            encode_binary_dataarray3_rank1_int32, &
            encode_binary_dataarray3_rank1_int16, &
            encode_binary_dataarray3_rank1_int8, &
            encode_binary_dataarray3_rank3_real64, &
            encode_binary_dataarray3_rank3_real32, &
            encode_binary_dataarray3_rank3_int64, &
            encode_binary_dataarray3_rank3_int32, &
            encode_binary_dataarray3_rank3_int16, &
            encode_binary_dataarray3_rank3_int8
    end interface encode_binary_dataarray
contains
    ! ascii encoder

    !| Encode (Base64) a dataarray with 1 components of rank 1 (real64.
    function encode_ascii_dataarray1_rank1_real128(x) result(code)
        real(real128), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))
        end do
    end function encode_ascii_dataarray1_rank1_real128

    !| Encode (Base64) a dataarray with 1 components of rank 1 (real64).
    function encode_ascii_dataarray1_rank1_real64(x) result(code)
        real(real64), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))
        end do
    end function encode_ascii_dataarray1_rank1_real64

    !| Encode (Base64) a dataarray with 1 components of rank 1 (real32).
    function encode_ascii_dataarray1_rank1_real32(x) result(code)
        real(real32), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))
        end do
    end function encode_ascii_dataarray1_rank1_real32

    !| Encode (Base64) a dataarray with 1 components of rank 1 (int64).
    function encode_ascii_dataarray1_rank1_int64(x) result(code)
        integer(int64), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))
        end do
    end function encode_ascii_dataarray1_rank1_int64

    !| Encode (Base64) a dataarray with 1 components of rank 1 (int32).
    function encode_ascii_dataarray1_rank1_int32(x) result(code)
        integer(int32), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))
        end do
    end function encode_ascii_dataarray1_rank1_int32

    !| Encode (Base64) a dataarray with 1 components of rank 1 (int16).
    function encode_ascii_dataarray1_rank1_int16(x) result(code)
        integer(int16), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))
        end do
    end function encode_ascii_dataarray1_rank1_int16

    !| Encode (Base64) a dataarray with 1 components of rank 1 (int8).
    function encode_ascii_dataarray1_rank1_int8(x) result(code)
        integer(int8), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))
        end do
    end function encode_ascii_dataarray1_rank1_int8

    !| Encode (Base64) a dataarray with 1 components of rank 2 (real128).
    function encode_ascii_dataarray1_rank2_real128(x) result(code)
        real(real128), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        code = ''
        do n2 = 1, size(x, dim=2)
            do n1 = 1, size(x, dim=1) - 1
                code = code//str(n=x(n1, n2))//' '
            end do
            code = code//' '//str(n=x(size(x, dim=1), n2))
        end do
    end function encode_ascii_dataarray1_rank2_real128

    !| Encode (Base64) a dataarray with 1 components of rank 2 (real64).
    function encode_ascii_dataarray1_rank2_real64(x) result(code)
        real(real64), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1 !! Counter.
        integer(int32) :: n2 !! Counter.

        code = ''
        do n2 = 1, size(x, dim=2)
            do n1 = 1, size(x, dim=1) - 1
                code = code//str(n=x(n1, n2))//' '
            end do
            code = code//' '//str(n=x(size(x, dim=1), n2))
        end do
    end function encode_ascii_dataarray1_rank2_real64

    !| Encode (Base64) a dataarray with 1 components of rank 2 (real32).
    function encode_ascii_dataarray1_rank2_real32(x) result(code)
        real(real32), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        code = ''
        do n2 = 1, size(x, dim=2)
            do n1 = 1, size(x, dim=1)
                code = code//str(n=x(n1, n2))//' '
            end do
        end do
    end function encode_ascii_dataarray1_rank2_real32

    !| Encode (Base64) a dataarray with 1 components of rank 2 (int64).
    function encode_ascii_dataarray1_rank2_int64(x) result(code)
        integer(int64), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        code = ''
        do n2 = 1, size(x, dim=2)
            do n1 = 1, size(x, dim=1) - 1
                code = code//str(n=x(n1, n2))//' '
            end do
            code = code//' '//str(n=x(size(x, dim=1), n2))
        end do
    end function encode_ascii_dataarray1_rank2_int64

    !| Encode (Base64) a dataarray with 1 components of rank 2 (int32).
    function encode_ascii_dataarray1_rank2_int32(x) result(code)
        integer(int32), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        code = ''
        do n2 = 1, size(x, dim=2)
            do n1 = 1, size(x, dim=1) - 1
                code = code//str(n=x(n1, n2))//' '
            end do
            code = code//' '//str(n=x(size(x, dim=1), n2))
        end do
    end function encode_ascii_dataarray1_rank2_int32

    !| Encode (Base64) a dataarray with 1 components of rank 2 (int16).
    function encode_ascii_dataarray1_rank2_int16(x) result(code)
        integer(int16), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        code = ''
        do n2 = 1, size(x, dim=2)
            do n1 = 1, size(x, dim=1) - 1
                code = code//str(n=x(n1, n2))//' '
            end do
            code = code//' '//str(n=x(size(x, dim=1), n2))
        end do
    end function encode_ascii_dataarray1_rank2_int16

    !| Encode (Base64) a dataarray with 1 components of rank 2 (int16).
    function encode_ascii_dataarray1_rank2_int8(x) result(code)
        integer(int8), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        code = ''
        do n2 = 1, size(x, dim=2)
            do n1 = 1, size(x, dim=1) - 1
                code = code//str(n=x(n1, n2))//' '
            end do
            code = code//' '//str(n=x(size(x, dim=1), n2))
        end do
    end function encode_ascii_dataarray1_rank2_int8

    !| Encode (Base64) a dataarray with 1 components of rank 3 (real128).
    function encode_ascii_dataarray1_rank3_real128(x) result(code)
        real(real128), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1) - 1
                    code = code//str(n=x(n1, n2, n3))//' '
                end do
                code = code//' '//str(n=x(size(x, dim=1), n2, n3))
            end do
        end do
    end function encode_ascii_dataarray1_rank3_real128

    !| Encode (Base64) a dataarray with 1 components of rank 3 (real64).
    function encode_ascii_dataarray1_rank3_real64(x) result(code)
        real(real64), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1) - 1
                    code = code//str(n=x(n1, n2, n3))//' '
                end do
                code = code//' '//str(n=x(size(x, dim=1), n2, n3))
            end do
        end do
    end function encode_ascii_dataarray1_rank3_real64

    !| Encode (Base64) a dataarray with 1 components of rank 3 (real32).
    function encode_ascii_dataarray1_rank3_real32(x) result(code)
        real(real32), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1) - 1
                    code = code//str(n=x(n1, n2, n3))//' '
                end do
                code = code//' '//str(n=x(size(x, dim=1), n2, n3))
            end do
        end do
    end function encode_ascii_dataarray1_rank3_real32

    !| Encode (Base64) a dataarray with 1 components of rank 3 (int64).
    function encode_ascii_dataarray1_rank3_int64(x) result(code)
        integer(int64), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1) - 1
                    code = code//str(n=x(n1, n2, n3))//' '
                end do
                code = code//' '//str(n=x(size(x, dim=1), n2, n3))
            end do
        end do
    end function encode_ascii_dataarray1_rank3_int64

    !| Encode (Base64) a dataarray with 1 components of rank 3 (int32).
    function encode_ascii_dataarray1_rank3_int32(x) result(code)
        integer(int32), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1) - 1
                    code = code//str(n=x(n1, n2, n3))//' '
                end do
                code = code//' '//str(n=x(size(x, dim=1), n2, n3))
            end do
        end do
    end function encode_ascii_dataarray1_rank3_int32

    !| Encode (Base64) a dataarray with 1 components of rank 3 (int16).
    function encode_ascii_dataarray1_rank3_int16(x) result(code)
        integer(int16), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1) - 1
                    code = code//str(n=x(n1, n2, n3))//' '
                end do
                code = code//' '//str(n=x(size(x, dim=1), n2, n3))
            end do
        end do
    end function encode_ascii_dataarray1_rank3_int16

    !| Encode (Base64) a dataarray with 1 components of rank 3 (int8).
    function encode_ascii_dataarray1_rank3_int8(x) result(code)
        integer(int8), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1) - 1
                    code = code//str(n=x(n1, n2, n3))//' '
                end do
                code = code//' '//str(n=x(size(x, dim=1), n2, n3))
            end do
        end do
    end function encode_ascii_dataarray1_rank3_int8

    !| Encode (Base64) a dataarray with 1 components of rank 4 (real128).
    function encode_ascii_dataarray1_rank4_real128(x) result(code)
        real(real128), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.
        integer(int32) :: n4
            !! Counter.

        code = ''
        do n4 = 1, size(x, dim=4)
            do n3 = 1, size(x, dim=3)
                do n2 = 1, size(x, dim=2)
                    do n1 = 1, size(x, dim=1)
                        code = code//str(n=x(n1, n2, n3, n4))//' '
                    end do
                end do
            end do
        end do
    end function encode_ascii_dataarray1_rank4_real128

    !| Encode (Base64) a dataarray with 1 components of rank 4 (real64).
    function encode_ascii_dataarray1_rank4_real64(x) result(code)
        real(real64), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.
        integer(int32) :: n4
            !! Counter.

        code = ''
        do n4 = 1, size(x, dim=4)
            do n3 = 1, size(x, dim=3)
                do n2 = 1, size(x, dim=2)
                    do n1 = 1, size(x, dim=1)
                        code = code//str(n=x(n1, n2, n3, n4))//' '
                    end do
                end do
            end do
        end do
    end function encode_ascii_dataarray1_rank4_real64

    !| Encode (Base64) a dataarray with 1 components of rank 4 (real32).
    function encode_ascii_dataarray1_rank4_real32(x) result(code)
        real(real32), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.
        integer(int32) :: n4
            !! Counter.

        code = ''
        do n4 = 1, size(x, dim=4)
            do n3 = 1, size(x, dim=3)
                do n2 = 1, size(x, dim=2)
                    do n1 = 1, size(x, dim=1)
                        code = code//str(n=x(n1, n2, n3, n4))//' '
                    end do
                end do
            end do
        end do
    end function encode_ascii_dataarray1_rank4_real32

    !| Encode (Base64) a dataarray with 1 components of rank 4 (int64).
    function encode_ascii_dataarray1_rank4_int64(x) result(code)
        integer(int64), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.
        integer(int32) :: n4
            !! Counter.

        code = ''
        do n4 = 1, size(x, dim=4)
            do n3 = 1, size(x, dim=3)
                do n2 = 1, size(x, dim=2)
                    do n1 = 1, size(x, dim=1)
                        code = code//str(n=x(n1, n2, n3, n4))//' '
                    end do
                end do
            end do
        end do
    end function encode_ascii_dataarray1_rank4_int64

    !| Encode (Base64) a dataarray with 1 components of rank 4 (int32).
    function encode_ascii_dataarray1_rank4_int32(x) result(code)
        integer(int32), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.
        integer(int32) :: n4
            !! Counter.

        code = ''
        do n4 = 1, size(x, dim=4)
            do n3 = 1, size(x, dim=3)
                do n2 = 1, size(x, dim=2)
                    do n1 = 1, size(x, dim=1)
                        code = code//str(n=x(n1, n2, n3, n4))//' '
                    end do
                end do
            end do
        end do
    end function encode_ascii_dataarray1_rank4_int32

    !| Encode (Base64) a dataarray with 1 components of rank 4 (int16).
    function encode_ascii_dataarray1_rank4_int16(x) result(code)
        integer(int16), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.
        integer(int32) :: n4
            !! Counter.

        code = ''
        do n4 = 1, size(x, dim=4)
            do n3 = 1, size(x, dim=3)
                do n2 = 1, size(x, dim=2)
                    do n1 = 1, size(x, dim=1)
                        code = code//str(n=x(n1, n2, n3, n4))//' '
                    end do
                end do
            end do
        end do
    end function encode_ascii_dataarray1_rank4_int16

    !| Encode (Base64) a dataarray with 1 components of rank 4 (int8).
    function encode_ascii_dataarray1_rank4_int8(x) result(code)
        integer(int8), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.
        integer(int32) :: n4
            !! Counter.

        code = ''
        do n4 = 1, size(x, dim=4)
            do n3 = 1, size(x, dim=3)
                do n2 = 1, size(x, dim=2)
                    do n1 = 1, size(x, dim=1)
                        code = code//str(n=x(n1, n2, n3, n4))//' '
                    end do
                end do
            end do
        end do
    end function encode_ascii_dataarray1_rank4_int8

    !| Encode (Base64) a dataarray with 3 components of rank 1 (real128).
    function encode_ascii_dataarray3_rank1_real128(x, y, z) result(code)
        real(real128), intent(in) :: x(1:)
            !! X component.
        real(real128), intent(in) :: y(1:)
            !! Y component.
        real(real128), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
        end do
    end function encode_ascii_dataarray3_rank1_real128

    !| Encode (Base64) a dataarray with 3 components of rank 1 (real64).
    function encode_ascii_dataarray3_rank1_real64(x, y, z) result(code)
        real(real64), intent(in) :: x(1:)
            !! X component.
        real(real64), intent(in) :: y(1:)
            !! Y component.
        real(real64), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
        end do
    end function encode_ascii_dataarray3_rank1_real64

    !| Encode (Base64) a dataarray with 3 components of rank 1 (real32).
    function encode_ascii_dataarray3_rank1_real32(x, y, z) result(code)
        real(real32), intent(in) :: x(1:)
            !! X component.
        real(real32), intent(in) :: y(1:)
            !! Y component.
        real(real32), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
        end do
    end function encode_ascii_dataarray3_rank1_real32

    !| Encode (Base64) a dataarray with 3 components of rank 1 (int64).
    function encode_ascii_dataarray3_rank1_int64(x, y, z) result(code)
        integer(int64), intent(in) :: x(1:)
            !! X component.
        integer(int64), intent(in) :: y(1:)
            !! Y component.
        integer(int64), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
        end do
    end function encode_ascii_dataarray3_rank1_int64

    !| Encode (Base64) a dataarray with 3 components of rank 1 (int32).
    function encode_ascii_dataarray3_rank1_int32(x, y, z) result(code)
        integer(int32), intent(in) :: x(1:)
            !! X component.
        integer(int32), intent(in) :: y(1:)
            !! Y component.
        integer(int32), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
        end do
    end function encode_ascii_dataarray3_rank1_int32

    !| Encode (Base64) a dataarray with 3 components of rank 1 (int16).
    function encode_ascii_dataarray3_rank1_int16(x, y, z) result(code)
        integer(int16), intent(in) :: x(1:)
            !! X component.
        integer(int16), intent(in) :: y(1:)
            !! Y component.
        integer(int16), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
        end do
    end function encode_ascii_dataarray3_rank1_int16

    !| Encode (Base64) a dataarray with 3 components of rank 1 (int8).
    function encode_ascii_dataarray3_rank1_int8(x, y, z) result(code)
        integer(int8), intent(in) :: x(1:)
            !! X component.
        integer(int8), intent(in) :: y(1:)
            !! Y component.
        integer(int8), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n
            !! Counter.

        code = ''
        do n = 1, size(x, dim=1)
            code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
        end do
    end function encode_ascii_dataarray3_rank1_int8

    !| Encode (Base64) a dataarray with 3 components of rank 3 (real64).
    function encode_ascii_dataarray3_rank3_real128(x, y, z) result(code)
        real(real128), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        real(real128), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        real(real128), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1)
                    code = code// &
                           str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
                end do
            end do
        end do
    end function encode_ascii_dataarray3_rank3_real128

    !| Encode (Base64) a dataarray with 3 components of rank 3 (real64).
    function encode_ascii_dataarray3_rank3_real64(x, y, z) result(code)
        real(real64), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        real(real64), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        real(real64), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1)
                    code = code// &
                           str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
                end do
            end do
        end do
    end function encode_ascii_dataarray3_rank3_real64

    !| Encode (Base64) a dataarray with 3 components of rank 3 (real32).
    function encode_ascii_dataarray3_rank3_real32(x, y, z) result(code)
        real(real32), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        real(real32), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        real(real32), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1)
                    code = code// &
                           str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
                end do
            end do
        end do
    end function encode_ascii_dataarray3_rank3_real32

    !| Encode (Base64) a dataarray with 3 components of rank 3 (int64).
    function encode_ascii_dataarray3_rank3_int64(x, y, z) result(code)
        integer(int64), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int64), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int64), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1)
                    code = code// &
                           str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
                end do
            end do
        end do
    end function encode_ascii_dataarray3_rank3_int64

    !| Encode (Base64) a dataarray with 3 components of rank 3 (int32).
    function encode_ascii_dataarray3_rank3_int32(x, y, z) result(code)
        integer(int32), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int32), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int32), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1)
                    code = code// &
                           str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
                end do
            end do
        end do
    end function encode_ascii_dataarray3_rank3_int32

    !| Encode (Base64) a dataarray with 3 components of rank 3 (int16).
    function encode_ascii_dataarray3_rank3_int16(x, y, z) result(code)
        integer(int16), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int16), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int16), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1)
                    code = code// &
                           str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
                end do
            end do
        end do
    end function encode_ascii_dataarray3_rank3_int16

    !| Encode (Base64) a dataarray with 3 components of rank 3 (int8).
    function encode_ascii_dataarray3_rank3_int8(x, y, z) result(code)
        integer(int8), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int8), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int8), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        code = ''
        do n3 = 1, size(x, dim=3)
            do n2 = 1, size(x, dim=2)
                do n1 = 1, size(x, dim=1)
                    code = code// &
                           str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
                end do
            end do
        end do
    end function encode_ascii_dataarray3_rank3_int8

    ! binary encoder
    !| Encode (Base64) a dataarray with 1 components of rank 1 (real64).
    function encode_binary_dataarray1_rank1_real64(x) result(code)
        real(real64), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)
        call pack_data(a1=[int(nn*BYR8P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank1_real64

    !| Encode (Base64) a dataarray with 1 components of rank 1 (real32).
    function encode_binary_dataarray1_rank1_real32(x) result(code)
        real(real32), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)
        call pack_data(a1=[int(nn*BYR4P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank1_real32

    !| Encode (Base64) a dataarray with 1 components of rank 1 (int64).
    function encode_binary_dataarray1_rank1_int64(x) result(code)
        integer(int64), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)
        call pack_data(a1=[int(nn*BYI8P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank1_int64

    !| Encode (Base64) a dataarray with 1 components of rank 1 (int32).
    function encode_binary_dataarray1_rank1_int32(x) result(code)
        integer(int32), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)
        xp = transfer([int(nn*BYI4P, int32), reshape(x, [nn])], xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank1_int32

    !| Encode (Base64) a dataarray with 1 components of rank 1 (int16).
    function encode_binary_dataarray1_rank1_int16(x) result(code)
        integer(int16), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)
        call pack_data(a1=[int(nn*BYI2P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank1_int16

    !| Encode (Base64) a dataarray with 1 components of rank 1 (int8).
    function encode_binary_dataarray1_rank1_int8(x) result(code)
        integer(int8), intent(in) :: x(1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)
        call pack_data(a1=[int(nn*BYI1P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank1_int8

    !| Encode (Base64) a dataarray with 1 components of rank 2 (real64).
    function encode_binary_dataarray1_rank2_real64(x) result(code)
        real(real64), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)
        call pack_data(a1=[int(nn*BYR8P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank2_real64

    !| Encode (Base64) a dataarray with 1 components of rank 2 (real32).
    function encode_binary_dataarray1_rank2_real32(x) result(code)
        real(real32), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)
        call pack_data(a1=[int(nn*BYR4P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank2_real32

    !| Encode (Base64) a dataarray with 1 components of rank 2 (int64).
    function encode_binary_dataarray1_rank2_int64(x) result(code)
        integer(int64), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)
        call pack_data(a1=[int(nn*BYI8P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank2_int64

    !| Encode (Base64) a dataarray with 1 components of rank 2 (int32).
    function encode_binary_dataarray1_rank2_int32(x) result(code)
        integer(int32), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)
        xp = transfer([int(nn*BYI4P, int32), reshape(x, [nn])], xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank2_int32

    !| Encode (Base64) a dataarray with 1 components of rank 2 (int16).
    function encode_binary_dataarray1_rank2_int16(x) result(code)
        integer(int16), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)
        call pack_data(a1=[int(nn*BYI2P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank2_int16

    !| Encode (Base64) a dataarray with 1 components of rank 2 (int8).
    function encode_binary_dataarray1_rank2_int8(x) result(code)
        integer(int8), intent(in) :: x(1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)
        call pack_data(a1=[int(nn*BYI1P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank2_int8

    !| Encode (Base64) a dataarray with 1 components of rank 3 (real64).
    function encode_binary_dataarray1_rank3_real64(x) result(code)
        real(real64), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
        call pack_data(a1=[int(nn*BYR8P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank3_real64

    !| Encode (Base64) a dataarray with 1 components of rank 3 (real32).
    function encode_binary_dataarray1_rank3_real32(x) result(code)
        real(real32), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
        call pack_data(a1=[int(nn*BYR4P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank3_real32

    !| Encode (Base64) a dataarray with 1 components of rank 3 (int64).
    function encode_binary_dataarray1_rank3_int64(x) result(code)
        integer(int64), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
        call pack_data(a1=[int(nn*BYI8P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank3_int64

    !| Encode (Base64) a dataarray with 1 components of rank 3 (int32).
    function encode_binary_dataarray1_rank3_int32(x) result(code)
        integer(int32), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
        xp = transfer([int(nn*BYI4P, int32), reshape(x, [nn])], xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank3_int32

    !| Encode (Base64) a dataarray with 1 components of rank 3 (int16).
    function encode_binary_dataarray1_rank3_int16(x) result(code)
        integer(int16), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
        call pack_data(a1=[int(nn*BYI2P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank3_int16

    !| Encode (Base64) a dataarray with 1 components of rank 3 (int8).
    function encode_binary_dataarray1_rank3_int8(x) result(code)
        integer(int8), intent(in) :: x(1:, 1:, 1:)
            !! Data variable
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
        call pack_data(a1=[int(nn*BYI1P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank3_int8

    !| Encode (Base64) a dataarray with 1 components of rank 4 (real64).
    function encode_binary_dataarray1_rank4_real64(x) result(code)
        real(real64), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
        call pack_data(a1=[int(nn*BYR8P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank4_real64

    !| Encode (Base64) a dataarray with 1 components of rank 4 (real32).
    function encode_binary_dataarray1_rank4_real32(x) result(code)
        real(real32), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
        call pack_data(a1=[int(nn*BYR4P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank4_real32

    !| Encode (Base64) a dataarray with 1 components of rank 4 (int64).
    function encode_binary_dataarray1_rank4_int64(x) result(code)
        integer(int64), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
        call pack_data(a1=[int(nn*BYI8P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank4_int64

    !| Encode (Base64) a dataarray with 1 components of rank 4 (int32).
    function encode_binary_dataarray1_rank4_int32(x) result(code)
        integer(int32), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
        xp = transfer([int(nn*BYI4P, int32), reshape(x, [nn])], xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank4_int32

    !| Encode (Base64) a dataarray with 1 components of rank 4 (int16).
    function encode_binary_dataarray1_rank4_int16(x) result(code)
        integer(int16), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
        call pack_data(a1=[int(nn*BYI2P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank4_int16

    !| Encode (Base64) a dataarray with 1 components of rank 4 (int8).
    function encode_binary_dataarray1_rank4_int8(x) result(code)
        integer(int8), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xp(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
        call pack_data(a1=[int(nn*BYI1P, int32)], a2=reshape(x, [nn]), packed=xp)
        call b64_encode(n=xp, code=code)
    end function encode_binary_dataarray1_rank4_int8

    !| Encode (Base64) a dataarray with 3 components of rank 1 (real64).
    function encode_binary_dataarray3_rank1_real64(x, y, z) result(code)
        real(real64), intent(in) :: x(1:)
            !! X component.
        real(real64), intent(in) :: y(1:)
            !! Y component.
        real(real64), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n
            !! Counter.

        nn = size(x, dim=1)
        call pack_data(a1=[int(3*nn*BYR8P, int32)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank1_real64

    !| Encode (Base64) a dataarray with 3 components of rank 1 (real32).
    function encode_binary_dataarray3_rank1_real32(x, y, z) result(code)
        real(real32), intent(in) :: x(1:)
            !! X component.
        real(real32), intent(in) :: y(1:)
            !! Y component.
        real(real32), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n
            !! Counter.

        nn = size(x, dim=1)
        call pack_data(a1=[int(3*nn*BYR4P, int32)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank1_real32

    !| Encode (Base64) a dataarray with 3 components of rank 1 (int64).
    function encode_binary_dataarray3_rank1_int64(x, y, z) result(code)
        integer(int64), intent(in) :: x(1:)
            !! X component.
        integer(int64), intent(in) :: y(1:)
            !! Y component.
        integer(int64), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n
            !! Counter.

        nn = size(x, dim=1)
        call pack_data(a1=[int(3*nn*BYI8P, int32)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank1_int64

    !| Encode (Base64) a dataarray with 3 components of rank 1 (int32).
    function encode_binary_dataarray3_rank1_int32(x, y, z) result(code)
        integer(int32), intent(in) :: x(1:)
            !! X component.
        integer(int32), intent(in) :: y(1:)
            !! Y component.
        integer(int32), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n
            !! Counter.

        nn = size(x, dim=1)
        xyz = transfer([int(3*nn*BYI4P, int32), [(x(n), y(n), z(n), n=1, nn)]], xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank1_int32

    !| Encode (Base64) a dataarray with 3 components of rank 1 (int16).
    function encode_binary_dataarray3_rank1_int16(x, y, z) result(code)
        integer(int16), intent(in) :: x(1:)
            !! X component.
        integer(int16), intent(in) :: y(1:)
            !! Y component.
        integer(int16), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n
            !! Counter.

        nn = size(x, dim=1)
        call pack_data(a1=[int(3*nn*BYI2P, int32)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank1_int16

    !| Encode (Base64) a dataarray with 3 components of rank 1 (int8).
    function encode_binary_dataarray3_rank1_int8(x, y, z) result(code)
        integer(int8), intent(in) :: x(1:)
            !! X component.
        integer(int8), intent(in) :: y(1:)
            !! Y component.
        integer(int8), intent(in) :: z(1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n
            !! Counter.

        nn = size(x, dim=1)
        call pack_data(a1=[int(3*nn*BYI1P, int32)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank1_int8

    !| Encode (Base64) a dataarray with 3 components of rank 3 (real64).
    function encode_binary_dataarray3_rank3_real64(x, y, z) result(code)
        real(real64), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        real(real64), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        real(real64), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn1
            !! Number of elements along dim 1.
        integer(int32) :: nn2
            !! Number of elements along dim 2.
        integer(int32) :: nn3
            !! Number of elements along dim 3.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        nn1 = size(x, dim=1)
        nn2 = size(x, dim=2)
        nn3 = size(x, dim=3)
        nn = nn1*nn2*nn3
        call pack_data(a1=[int(3*nn*BYR8P, int32)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1), &
                                                          n2=1, nn2), &
                                                         n3=1, nn3)], &
                       packed=xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank3_real64

    !| Encode (Base64) a dataarray with 3 components of rank 3 (real32).
    function encode_binary_dataarray3_rank3_real32(x, y, z) result(code)
        real(real32), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        real(real32), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        real(real32), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn1
            !! Number of elements along dim 1.
        integer(int32) :: nn2
            !! Number of elements along dim 2.
        integer(int32) :: nn3
            !! Number of elements along dim 3.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        nn1 = size(x, dim=1)
        nn2 = size(x, dim=2)
        nn3 = size(x, dim=3)
        nn = nn1*nn2*nn3
        call pack_data(a1=[int(3*nn*BYR4P, int32)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1), &
                                                          n2=1, nn2), &
                                                         n3=1, nn3)], &
                       packed=xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank3_real32

    !| Encode (Base64) a dataarray with 3 components of rank 3 (int64).
    function encode_binary_dataarray3_rank3_int64(x, y, z) result(code)
        integer(int64), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int64), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int64), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn1
            !! Number of elements along dim 1.
        integer(int32) :: nn2
            !! Number of elements along dim 2.
        integer(int32) :: nn3
            !! Number of elements along dim 3.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        nn1 = size(x, dim=1)
        nn2 = size(x, dim=2)
        nn3 = size(x, dim=3)
        nn = nn1*nn2*nn3
        call pack_data(a1=[int(3*nn*BYI8P, int32)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1), &
                                                          n2=1, nn2), &
                                                         n3=1, nn3)], &
                       packed=xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank3_int64

    !| Encode (Base64) a dataarray with 3 components of rank 3 (int32).
    function encode_binary_dataarray3_rank3_int32(x, y, z) result(code)
        integer(int32), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int32), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int32), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn1
            !! Number of elements along dim 1.
        integer(int32) :: nn2
            !! Number of elements along dim 2.
        integer(int32) :: nn3
            !! Number of elements along dim 3.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        nn1 = size(x, dim=1)
        nn2 = size(x, dim=2)
        nn3 = size(x, dim=3)
        nn = nn1*nn2*nn3
        xyz = transfer([int(3*nn*BYI4P, int32), [(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1), &
                                                   n2=1, nn2), &
                                                  n3=1, nn3)]], xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank3_int32

    !| Encode (Base64) a dataarray with 3 components of rank 3 (int16).
    function encode_binary_dataarray3_rank3_int16(x, y, z) result(code)
        integer(int16), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int16), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int16), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn1
            !! Number of elements along dim 1.
        integer(int32) :: nn2
            !! Number of elements along dim 2.
        integer(int32) :: nn3
            !! Number of elements along dim 3.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        nn1 = size(x, dim=1)
        nn2 = size(x, dim=2)
        nn3 = size(x, dim=3)
        nn = nn1*nn2*nn3
        call pack_data(a1=[int(3*nn*BYI2P, int32)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1), &
                                                          n2=1, nn2), &
                                                         n3=1, nn3)], &
                       packed=xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank3_int16

    !| Encode (Base64) a dataarray with 3 components of rank 3 (int8).
    function encode_binary_dataarray3_rank3_int8(x, y, z) result(code)
        integer(int8), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int8), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int8), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        character(len=:), allocatable :: code
            !! Encoded base64 dataarray.
        integer(int8), allocatable :: xyz(:)
            !! Packed data.
        integer(int32) :: nn1
            !! Number of elements along dim 1.
        integer(int32) :: nn2
            !! Number of elements along dim 2.
        integer(int32) :: nn3
            !! Number of elements along dim 3.
        integer(int32) :: nn
            !! Number of elements.
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        nn1 = size(x, dim=1)
        nn2 = size(x, dim=2)
        nn3 = size(x, dim=3)
        nn = nn1*nn2*nn3
        call pack_data(a1=[int(3*nn*BYI1P, int32)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1), &
                                                          n2=1, nn2), &
                                                         n3=1, nn3)], &
                       packed=xyz)
        call b64_encode(n=xyz, code=code)
    end function encode_binary_dataarray3_rank3_int8
end module vtk_fortran_dataarray_encoder
