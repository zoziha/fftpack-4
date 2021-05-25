
    subroutine radf3_sp (ido, l1, cc, ch, wa1, wa2)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension ch(ido, 3, l1), cc(ido, l1, 3), &
            wa1(*), wa2(*)
!     *** taui is -sqrt(3)/2 ***
        data taur, taui/-0.5d0, 0.86602540378443864676d0/
        do k = 1, l1
            cr2 = cc(1, k, 2) + cc(1, k, 3)
            ch(1, 1, k) = cc(1, k, 1) + cr2
            ch(1, 3, k) = taui*(cc(1, k, 3) - cc(1, k, 2))
            ch(ido, 2, k) = cc(1, k, 1) + taur*cr2
        end do
        if (ido .eq. 1) return
        idp2 = ido + 2
        do k = 1, l1
            do i = 3, ido, 2
                ic = idp2 - i
                dr2 = wa1(i - 2)*cc(i - 1, k, 2) + wa1(i - 1)*cc(i, k, 2)
                di2 = wa1(i - 2)*cc(i, k, 2) - wa1(i - 1)*cc(i - 1, k, 2)
                dr3 = wa2(i - 2)*cc(i - 1, k, 3) + wa2(i - 1)*cc(i, k, 3)
                di3 = wa2(i - 2)*cc(i, k, 3) - wa2(i - 1)*cc(i - 1, k, 3)
                cr2 = dr2 + dr3
                ci2 = di2 + di3
                ch(i - 1, 1, k) = cc(i - 1, k, 1) + cr2
                ch(i, 1, k) = cc(i, k, 1) + ci2
                tr2 = cc(i - 1, k, 1) + taur*cr2
                ti2 = cc(i, k, 1) + taur*ci2
                tr3 = taui*(di2 - di3)
                ti3 = taui*(dr3 - dr2)
                ch(i - 1, 3, k) = tr2 + tr3
                ch(ic - 1, 2, k) = tr2 - tr3
                ch(i, 3, k) = ti2 + ti3
                ch(ic, 2, k) = ti3 - ti2
            end do
        end do
        return
    end subroutine

    subroutine radf3_dp (ido, l1, cc, ch, wa1, wa2)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension ch(ido, 3, l1), cc(ido, l1, 3), &
            wa1(*), wa2(*)
!     *** taui is -sqrt(3)/2 ***
        data taur, taui/-0.5d0, 0.86602540378443864676d0/
        do k = 1, l1
            cr2 = cc(1, k, 2) + cc(1, k, 3)
            ch(1, 1, k) = cc(1, k, 1) + cr2
            ch(1, 3, k) = taui*(cc(1, k, 3) - cc(1, k, 2))
            ch(ido, 2, k) = cc(1, k, 1) + taur*cr2
        end do
        if (ido .eq. 1) return
        idp2 = ido + 2
        do k = 1, l1
            do i = 3, ido, 2
                ic = idp2 - i
                dr2 = wa1(i - 2)*cc(i - 1, k, 2) + wa1(i - 1)*cc(i, k, 2)
                di2 = wa1(i - 2)*cc(i, k, 2) - wa1(i - 1)*cc(i - 1, k, 2)
                dr3 = wa2(i - 2)*cc(i - 1, k, 3) + wa2(i - 1)*cc(i, k, 3)
                di3 = wa2(i - 2)*cc(i, k, 3) - wa2(i - 1)*cc(i - 1, k, 3)
                cr2 = dr2 + dr3
                ci2 = di2 + di3
                ch(i - 1, 1, k) = cc(i - 1, k, 1) + cr2
                ch(i, 1, k) = cc(i, k, 1) + ci2
                tr2 = cc(i - 1, k, 1) + taur*cr2
                ti2 = cc(i, k, 1) + taur*ci2
                tr3 = taui*(di2 - di3)
                ti3 = taui*(dr3 - dr2)
                ch(i - 1, 3, k) = tr2 + tr3
                ch(ic - 1, 2, k) = tr2 - tr3
                ch(i, 3, k) = ti2 + ti3
                ch(ic, 2, k) = ti3 - ti2
            end do
        end do
        return
    end subroutine

    subroutine radf3_qp (ido, l1, cc, ch, wa1, wa2)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension ch(ido, 3, l1), cc(ido, l1, 3), &
            wa1(*), wa2(*)
!     *** taui is -sqrt(3)/2 ***
        data taur, taui/-0.5d0, 0.86602540378443864676d0/
        do k = 1, l1
            cr2 = cc(1, k, 2) + cc(1, k, 3)
            ch(1, 1, k) = cc(1, k, 1) + cr2
            ch(1, 3, k) = taui*(cc(1, k, 3) - cc(1, k, 2))
            ch(ido, 2, k) = cc(1, k, 1) + taur*cr2
        end do
        if (ido .eq. 1) return
        idp2 = ido + 2
        do k = 1, l1
            do i = 3, ido, 2
                ic = idp2 - i
                dr2 = wa1(i - 2)*cc(i - 1, k, 2) + wa1(i - 1)*cc(i, k, 2)
                di2 = wa1(i - 2)*cc(i, k, 2) - wa1(i - 1)*cc(i - 1, k, 2)
                dr3 = wa2(i - 2)*cc(i - 1, k, 3) + wa2(i - 1)*cc(i, k, 3)
                di3 = wa2(i - 2)*cc(i, k, 3) - wa2(i - 1)*cc(i - 1, k, 3)
                cr2 = dr2 + dr3
                ci2 = di2 + di3
                ch(i - 1, 1, k) = cc(i - 1, k, 1) + cr2
                ch(i, 1, k) = cc(i, k, 1) + ci2
                tr2 = cc(i - 1, k, 1) + taur*cr2
                ti2 = cc(i, k, 1) + taur*ci2
                tr3 = taui*(di2 - di3)
                ti3 = taui*(dr3 - dr2)
                ch(i - 1, 3, k) = tr2 + tr3
                ch(ic - 1, 2, k) = tr2 - tr3
                ch(i, 3, k) = ti2 + ti3
                ch(ic, 2, k) = ti3 - ti2
            end do
        end do
        return
    end subroutine
