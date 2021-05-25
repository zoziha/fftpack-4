    subroutine passb2_sp (ido, l1, cc, ch, wa1)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension cc(ido, 2, l1), ch(ido, l1, 2), &
            wa1(*)
        if (ido .gt. 2) go to 102
        do k = 1, l1
            ch(1, k, 1) = cc(1, 1, k) + cc(1, 2, k)
            ch(1, k, 2) = cc(1, 1, k) - cc(1, 2, k)
            ch(2, k, 1) = cc(2, 1, k) + cc(2, 2, k)
            ch(2, k, 2) = cc(2, 1, k) - cc(2, 2, k)
        end do
        return
102     do k = 1, l1
            do i = 2, ido, 2
                ch(i - 1, k, 1) = cc(i - 1, 1, k) + cc(i - 1, 2, k)
                tr2 = cc(i - 1, 1, k) - cc(i - 1, 2, k)
                ch(i, k, 1) = cc(i, 1, k) + cc(i, 2, k)
                ti2 = cc(i, 1, k) - cc(i, 2, k)
                ch(i, k, 2) = wa1(i - 1)*ti2 + wa1(i)*tr2
                ch(i - 1, k, 2) = wa1(i - 1)*tr2 - wa1(i)*ti2
            end do
        end do
        return
    end subroutine

    subroutine passb2_dp (ido, l1, cc, ch, wa1)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension cc(ido, 2, l1), ch(ido, l1, 2), &
            wa1(*)
        if (ido .gt. 2) go to 102
        do k = 1, l1
            ch(1, k, 1) = cc(1, 1, k) + cc(1, 2, k)
            ch(1, k, 2) = cc(1, 1, k) - cc(1, 2, k)
            ch(2, k, 1) = cc(2, 1, k) + cc(2, 2, k)
            ch(2, k, 2) = cc(2, 1, k) - cc(2, 2, k)
        end do
        return
102     do k = 1, l1
            do i = 2, ido, 2
                ch(i - 1, k, 1) = cc(i - 1, 1, k) + cc(i - 1, 2, k)
                tr2 = cc(i - 1, 1, k) - cc(i - 1, 2, k)
                ch(i, k, 1) = cc(i, 1, k) + cc(i, 2, k)
                ti2 = cc(i, 1, k) - cc(i, 2, k)
                ch(i, k, 2) = wa1(i - 1)*ti2 + wa1(i)*tr2
                ch(i - 1, k, 2) = wa1(i - 1)*tr2 - wa1(i)*ti2
            end do
        end do
        return
    end subroutine

    subroutine passb2_qp (ido, l1, cc, ch, wa1)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension cc(ido, 2, l1), ch(ido, l1, 2), &
            wa1(*)
        if (ido .gt. 2) go to 102
        do k = 1, l1
            ch(1, k, 1) = cc(1, 1, k) + cc(1, 2, k)
            ch(1, k, 2) = cc(1, 1, k) - cc(1, 2, k)
            ch(2, k, 1) = cc(2, 1, k) + cc(2, 2, k)
            ch(2, k, 2) = cc(2, 1, k) - cc(2, 2, k)
        end do
        return
102     do k = 1, l1
            do i = 2, ido, 2
                ch(i - 1, k, 1) = cc(i - 1, 1, k) + cc(i - 1, 2, k)
                tr2 = cc(i - 1, 1, k) - cc(i - 1, 2, k)
                ch(i, k, 1) = cc(i, 1, k) + cc(i, 2, k)
                ti2 = cc(i, 1, k) - cc(i, 2, k)
                ch(i, k, 2) = wa1(i - 1)*ti2 + wa1(i)*tr2
                ch(i - 1, k, 2) = wa1(i - 1)*tr2 - wa1(i)*ti2
            end do
        end do
        return
    end subroutine


