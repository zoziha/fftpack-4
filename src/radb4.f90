
    subroutine radb4_sp (ido, l1, cc, ch, wa1, wa2, wa3)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension cc(ido, 4, l1), ch(ido, l1, 4), &
            wa1(*), wa2(*), wa3(*)
        data sqrt2/1.41421356237309504880d0/
        do k = 1, l1
            tr1 = cc(1, 1, k) - cc(ido, 4, k)
            tr2 = cc(1, 1, k) + cc(ido, 4, k)
            tr3 = cc(ido, 2, k) + cc(ido, 2, k)
            tr4 = cc(1, 3, k) + cc(1, 3, k)
            ch(1, k, 1) = tr2 + tr3
            ch(1, k, 2) = tr1 - tr4
            ch(1, k, 3) = tr2 - tr3
            ch(1, k, 4) = tr1 + tr4
        end do
        if (ido - 2) 107, 105, 102
102     idp2 = ido + 2
        do k = 1, l1
            do i = 3, ido, 2
                ic = idp2 - i
                ti1 = cc(i, 1, k) + cc(ic, 4, k)
                ti2 = cc(i, 1, k) - cc(ic, 4, k)
                ti3 = cc(i, 3, k) - cc(ic, 2, k)
                tr4 = cc(i, 3, k) + cc(ic, 2, k)
                tr1 = cc(i - 1, 1, k) - cc(ic - 1, 4, k)
                tr2 = cc(i - 1, 1, k) + cc(ic - 1, 4, k)
                ti4 = cc(i - 1, 3, k) - cc(ic - 1, 2, k)
                tr3 = cc(i - 1, 3, k) + cc(ic - 1, 2, k)
                ch(i - 1, k, 1) = tr2 + tr3
                cr3 = tr2 - tr3
                ch(i, k, 1) = ti2 + ti3
                ci3 = ti2 - ti3
                cr2 = tr1 - tr4
                cr4 = tr1 + tr4
                ci2 = ti1 + ti4
                ci4 = ti1 - ti4
                ch(i - 1, k, 2) = wa1(i - 2)*cr2 - wa1(i - 1)*ci2
                ch(i, k, 2) = wa1(i - 2)*ci2 + wa1(i - 1)*cr2
                ch(i - 1, k, 3) = wa2(i - 2)*cr3 - wa2(i - 1)*ci3
                ch(i, k, 3) = wa2(i - 2)*ci3 + wa2(i - 1)*cr3
                ch(i - 1, k, 4) = wa3(i - 2)*cr4 - wa3(i - 1)*ci4
                ch(i, k, 4) = wa3(i - 2)*ci4 + wa3(i - 1)*cr4
            end do
        end do
        if (mod(ido, 2) .eq. 1) return
105     continue
        do k = 1, l1
            ti1 = cc(1, 2, k) + cc(1, 4, k)
            ti2 = cc(1, 4, k) - cc(1, 2, k)
            tr1 = cc(ido, 1, k) - cc(ido, 3, k)
            tr2 = cc(ido, 1, k) + cc(ido, 3, k)
            ch(ido, k, 1) = tr2 + tr2
            ch(ido, k, 2) = sqrt2*(tr1 - ti1)
            ch(ido, k, 3) = ti2 + ti2
            ch(ido, k, 4) = -sqrt2*(tr1 + ti1)
        end do
107     return
    end subroutine

    subroutine radb4_dp (ido, l1, cc, ch, wa1, wa2, wa3)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension cc(ido, 4, l1), ch(ido, l1, 4), &
            wa1(*), wa2(*), wa3(*)
        data sqrt2/1.41421356237309504880d0/
        do k = 1, l1
            tr1 = cc(1, 1, k) - cc(ido, 4, k)
            tr2 = cc(1, 1, k) + cc(ido, 4, k)
            tr3 = cc(ido, 2, k) + cc(ido, 2, k)
            tr4 = cc(1, 3, k) + cc(1, 3, k)
            ch(1, k, 1) = tr2 + tr3
            ch(1, k, 2) = tr1 - tr4
            ch(1, k, 3) = tr2 - tr3
            ch(1, k, 4) = tr1 + tr4
        end do
        if (ido - 2) 107, 105, 102
102     idp2 = ido + 2
        do k = 1, l1
            do i = 3, ido, 2
                ic = idp2 - i
                ti1 = cc(i, 1, k) + cc(ic, 4, k)
                ti2 = cc(i, 1, k) - cc(ic, 4, k)
                ti3 = cc(i, 3, k) - cc(ic, 2, k)
                tr4 = cc(i, 3, k) + cc(ic, 2, k)
                tr1 = cc(i - 1, 1, k) - cc(ic - 1, 4, k)
                tr2 = cc(i - 1, 1, k) + cc(ic - 1, 4, k)
                ti4 = cc(i - 1, 3, k) - cc(ic - 1, 2, k)
                tr3 = cc(i - 1, 3, k) + cc(ic - 1, 2, k)
                ch(i - 1, k, 1) = tr2 + tr3
                cr3 = tr2 - tr3
                ch(i, k, 1) = ti2 + ti3
                ci3 = ti2 - ti3
                cr2 = tr1 - tr4
                cr4 = tr1 + tr4
                ci2 = ti1 + ti4
                ci4 = ti1 - ti4
                ch(i - 1, k, 2) = wa1(i - 2)*cr2 - wa1(i - 1)*ci2
                ch(i, k, 2) = wa1(i - 2)*ci2 + wa1(i - 1)*cr2
                ch(i - 1, k, 3) = wa2(i - 2)*cr3 - wa2(i - 1)*ci3
                ch(i, k, 3) = wa2(i - 2)*ci3 + wa2(i - 1)*cr3
                ch(i - 1, k, 4) = wa3(i - 2)*cr4 - wa3(i - 1)*ci4
                ch(i, k, 4) = wa3(i - 2)*ci4 + wa3(i - 1)*cr4
            end do
        end do
        if (mod(ido, 2) .eq. 1) return
105     continue
        do k = 1, l1
            ti1 = cc(1, 2, k) + cc(1, 4, k)
            ti2 = cc(1, 4, k) - cc(1, 2, k)
            tr1 = cc(ido, 1, k) - cc(ido, 3, k)
            tr2 = cc(ido, 1, k) + cc(ido, 3, k)
            ch(ido, k, 1) = tr2 + tr2
            ch(ido, k, 2) = sqrt2*(tr1 - ti1)
            ch(ido, k, 3) = ti2 + ti2
            ch(ido, k, 4) = -sqrt2*(tr1 + ti1)
        end do
107     return
    end subroutine

    subroutine radb4_qp (ido, l1, cc, ch, wa1, wa2, wa3)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension cc(ido, 4, l1), ch(ido, l1, 4), &
            wa1(*), wa2(*), wa3(*)
        data sqrt2/1.41421356237309504880d0/
        do k = 1, l1
            tr1 = cc(1, 1, k) - cc(ido, 4, k)
            tr2 = cc(1, 1, k) + cc(ido, 4, k)
            tr3 = cc(ido, 2, k) + cc(ido, 2, k)
            tr4 = cc(1, 3, k) + cc(1, 3, k)
            ch(1, k, 1) = tr2 + tr3
            ch(1, k, 2) = tr1 - tr4
            ch(1, k, 3) = tr2 - tr3
            ch(1, k, 4) = tr1 + tr4
        end do
        if (ido - 2) 107, 105, 102
102     idp2 = ido + 2
        do k = 1, l1
            do i = 3, ido, 2
                ic = idp2 - i
                ti1 = cc(i, 1, k) + cc(ic, 4, k)
                ti2 = cc(i, 1, k) - cc(ic, 4, k)
                ti3 = cc(i, 3, k) - cc(ic, 2, k)
                tr4 = cc(i, 3, k) + cc(ic, 2, k)
                tr1 = cc(i - 1, 1, k) - cc(ic - 1, 4, k)
                tr2 = cc(i - 1, 1, k) + cc(ic - 1, 4, k)
                ti4 = cc(i - 1, 3, k) - cc(ic - 1, 2, k)
                tr3 = cc(i - 1, 3, k) + cc(ic - 1, 2, k)
                ch(i - 1, k, 1) = tr2 + tr3
                cr3 = tr2 - tr3
                ch(i, k, 1) = ti2 + ti3
                ci3 = ti2 - ti3
                cr2 = tr1 - tr4
                cr4 = tr1 + tr4
                ci2 = ti1 + ti4
                ci4 = ti1 - ti4
                ch(i - 1, k, 2) = wa1(i - 2)*cr2 - wa1(i - 1)*ci2
                ch(i, k, 2) = wa1(i - 2)*ci2 + wa1(i - 1)*cr2
                ch(i - 1, k, 3) = wa2(i - 2)*cr3 - wa2(i - 1)*ci3
                ch(i, k, 3) = wa2(i - 2)*ci3 + wa2(i - 1)*cr3
                ch(i - 1, k, 4) = wa3(i - 2)*cr4 - wa3(i - 1)*ci4
                ch(i, k, 4) = wa3(i - 2)*ci4 + wa3(i - 1)*cr4
            end do
        end do
        if (mod(ido, 2) .eq. 1) return
105     continue
        do k = 1, l1
            ti1 = cc(1, 2, k) + cc(1, 4, k)
            ti2 = cc(1, 4, k) - cc(1, 2, k)
            tr1 = cc(ido, 1, k) - cc(ido, 3, k)
            tr2 = cc(ido, 1, k) + cc(ido, 3, k)
            ch(ido, k, 1) = tr2 + tr2
            ch(ido, k, 2) = sqrt2*(tr1 - ti1)
            ch(ido, k, 3) = ti2 + ti2
            ch(ido, k, 4) = -sqrt2*(tr1 + ti1)
        end do
107     return
    end subroutine

