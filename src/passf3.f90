
    subroutine passf3_sp (ido, l1, cc, ch, wa1, wa2)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension cc(ido, 3, l1), ch(ido, l1, 3), &
            wa1(*), wa2(*)
!     *** taui is -sqrt(3)/2 ***
        data taur, taui/-0.5d0, -0.86602540378443864676d0/
        if (ido .ne. 2) go to 102
        do k = 1, l1
            tr2 = cc(1, 2, k) + cc(1, 3, k)
            cr2 = cc(1, 1, k) + taur*tr2
            ch(1, k, 1) = cc(1, 1, k) + tr2
            ti2 = cc(2, 2, k) + cc(2, 3, k)
            ci2 = cc(2, 1, k) + taur*ti2
            ch(2, k, 1) = cc(2, 1, k) + ti2
            cr3 = taui*(cc(1, 2, k) - cc(1, 3, k))
            ci3 = taui*(cc(2, 2, k) - cc(2, 3, k))
            ch(1, k, 2) = cr2 - ci3
            ch(1, k, 3) = cr2 + ci3
            ch(2, k, 2) = ci2 + cr3
            ch(2, k, 3) = ci2 - cr3
        end do
        return
102     do k = 1, l1
            do i = 2, ido, 2
                tr2 = cc(i - 1, 2, k) + cc(i - 1, 3, k)
                cr2 = cc(i - 1, 1, k) + taur*tr2
                ch(i - 1, k, 1) = cc(i - 1, 1, k) + tr2
                ti2 = cc(i, 2, k) + cc(i, 3, k)
                ci2 = cc(i, 1, k) + taur*ti2
                ch(i, k, 1) = cc(i, 1, k) + ti2
                cr3 = taui*(cc(i - 1, 2, k) - cc(i - 1, 3, k))
                ci3 = taui*(cc(i, 2, k) - cc(i, 3, k))
                dr2 = cr2 - ci3
                dr3 = cr2 + ci3
                di2 = ci2 + cr3
                di3 = ci2 - cr3
                ch(i, k, 2) = wa1(i - 1)*di2 - wa1(i)*dr2
                ch(i - 1, k, 2) = wa1(i - 1)*dr2 + wa1(i)*di2
                ch(i, k, 3) = wa2(i - 1)*di3 - wa2(i)*dr3
                ch(i - 1, k, 3) = wa2(i - 1)*dr3 + wa2(i)*di3
            end do
        end do
        return
    end subroutine

    subroutine passf3_dp (ido, l1, cc, ch, wa1, wa2)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension cc(ido, 3, l1), ch(ido, l1, 3), &
            wa1(*), wa2(*)
!     *** taui is -sqrt(3)/2 ***
        data taur, taui/-0.5d0, -0.86602540378443864676d0/
        if (ido .ne. 2) go to 102
        do k = 1, l1
            tr2 = cc(1, 2, k) + cc(1, 3, k)
            cr2 = cc(1, 1, k) + taur*tr2
            ch(1, k, 1) = cc(1, 1, k) + tr2
            ti2 = cc(2, 2, k) + cc(2, 3, k)
            ci2 = cc(2, 1, k) + taur*ti2
            ch(2, k, 1) = cc(2, 1, k) + ti2
            cr3 = taui*(cc(1, 2, k) - cc(1, 3, k))
            ci3 = taui*(cc(2, 2, k) - cc(2, 3, k))
            ch(1, k, 2) = cr2 - ci3
            ch(1, k, 3) = cr2 + ci3
            ch(2, k, 2) = ci2 + cr3
            ch(2, k, 3) = ci2 - cr3
        end do
        return
102     do k = 1, l1
            do i = 2, ido, 2
                tr2 = cc(i - 1, 2, k) + cc(i - 1, 3, k)
                cr2 = cc(i - 1, 1, k) + taur*tr2
                ch(i - 1, k, 1) = cc(i - 1, 1, k) + tr2
                ti2 = cc(i, 2, k) + cc(i, 3, k)
                ci2 = cc(i, 1, k) + taur*ti2
                ch(i, k, 1) = cc(i, 1, k) + ti2
                cr3 = taui*(cc(i - 1, 2, k) - cc(i - 1, 3, k))
                ci3 = taui*(cc(i, 2, k) - cc(i, 3, k))
                dr2 = cr2 - ci3
                dr3 = cr2 + ci3
                di2 = ci2 + cr3
                di3 = ci2 - cr3
                ch(i, k, 2) = wa1(i - 1)*di2 - wa1(i)*dr2
                ch(i - 1, k, 2) = wa1(i - 1)*dr2 + wa1(i)*di2
                ch(i, k, 3) = wa2(i - 1)*di3 - wa2(i)*dr3
                ch(i - 1, k, 3) = wa2(i - 1)*dr3 + wa2(i)*di3
            end do
        end do
        return
    end subroutine

    subroutine passf3_qp (ido, l1, cc, ch, wa1, wa2)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension cc(ido, 3, l1), ch(ido, l1, 3), &
            wa1(*), wa2(*)
!     *** taui is -sqrt(3)/2 ***
        data taur, taui/-0.5d0, -0.86602540378443864676d0/
        if (ido .ne. 2) go to 102
        do k = 1, l1
            tr2 = cc(1, 2, k) + cc(1, 3, k)
            cr2 = cc(1, 1, k) + taur*tr2
            ch(1, k, 1) = cc(1, 1, k) + tr2
            ti2 = cc(2, 2, k) + cc(2, 3, k)
            ci2 = cc(2, 1, k) + taur*ti2
            ch(2, k, 1) = cc(2, 1, k) + ti2
            cr3 = taui*(cc(1, 2, k) - cc(1, 3, k))
            ci3 = taui*(cc(2, 2, k) - cc(2, 3, k))
            ch(1, k, 2) = cr2 - ci3
            ch(1, k, 3) = cr2 + ci3
            ch(2, k, 2) = ci2 + cr3
            ch(2, k, 3) = ci2 - cr3
        end do
        return
102     do k = 1, l1
            do i = 2, ido, 2
                tr2 = cc(i - 1, 2, k) + cc(i - 1, 3, k)
                cr2 = cc(i - 1, 1, k) + taur*tr2
                ch(i - 1, k, 1) = cc(i - 1, 1, k) + tr2
                ti2 = cc(i, 2, k) + cc(i, 3, k)
                ci2 = cc(i, 1, k) + taur*ti2
                ch(i, k, 1) = cc(i, 1, k) + ti2
                cr3 = taui*(cc(i - 1, 2, k) - cc(i - 1, 3, k))
                ci3 = taui*(cc(i, 2, k) - cc(i, 3, k))
                dr2 = cr2 - ci3
                dr3 = cr2 + ci3
                di2 = ci2 + cr3
                di3 = ci2 - cr3
                ch(i, k, 2) = wa1(i - 1)*di2 - wa1(i)*dr2
                ch(i - 1, k, 2) = wa1(i - 1)*dr2 + wa1(i)*di2
                ch(i, k, 3) = wa2(i - 1)*di3 - wa2(i)*dr3
                ch(i - 1, k, 3) = wa2(i - 1)*dr3 + wa2(i)*di3
            end do
        end do
        return
    end subroutine
