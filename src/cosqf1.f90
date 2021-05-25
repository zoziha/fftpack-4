
    subroutine cosqf1_sp (n, x, w, xh)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension x(*), w(*), xh(*)
        ns2 = (n + 1)/2
        np2 = n + 2
        do k = 2, ns2
            kc = np2 - k
            xh(k) = x(k) + x(kc)
            xh(kc) = x(k) - x(kc)
        end do
        modn = mod(n, 2)
        if (modn .eq. 0) xh(ns2 + 1) = x(ns2 + 1) + x(ns2 + 1)
        do k = 2, ns2
            kc = np2 - k
            x(k) = w(k - 1)*xh(kc) + w(kc - 1)*xh(k)
            x(kc) = w(k - 1)*xh(k) - w(kc - 1)*xh(kc)
        end do
        if (modn .eq. 0) x(ns2 + 1) = w(ns2)*xh(ns2 + 1)
        call dfftf_sp(n, x, xh)
        do i = 3, n, 2
            xim1 = x(i - 1) - x(i)
            x(i) = x(i - 1) + x(i)
            x(i - 1) = xim1
        end do
        return
    end subroutine

    subroutine cosqf1_dp (n, x, w, xh)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension x(*), w(*), xh(*)
        ns2 = (n + 1)/2
        np2 = n + 2
        do k = 2, ns2
            kc = np2 - k
            xh(k) = x(k) + x(kc)
            xh(kc) = x(k) - x(kc)
        end do
        modn = mod(n, 2)
        if (modn .eq. 0) xh(ns2 + 1) = x(ns2 + 1) + x(ns2 + 1)
        do k = 2, ns2
            kc = np2 - k
            x(k) = w(k - 1)*xh(kc) + w(kc - 1)*xh(k)
            x(kc) = w(k - 1)*xh(k) - w(kc - 1)*xh(kc)
        end do
        if (modn .eq. 0) x(ns2 + 1) = w(ns2)*xh(ns2 + 1)
        call dfftf_dp(n, x, xh)
        do i = 3, n, 2
            xim1 = x(i - 1) - x(i)
            x(i) = x(i - 1) + x(i)
            x(i - 1) = xim1
        end do
        return
    end subroutine

    subroutine cosqf1_qp (n, x, w, xh)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension x(*), w(*), xh(*)
        ns2 = (n + 1)/2
        np2 = n + 2
        do k = 2, ns2
            kc = np2 - k
            xh(k) = x(k) + x(kc)
            xh(kc) = x(k) - x(kc)
        end do
        modn = mod(n, 2)
        if (modn .eq. 0) xh(ns2 + 1) = x(ns2 + 1) + x(ns2 + 1)
        do k = 2, ns2
            kc = np2 - k
            x(k) = w(k - 1)*xh(kc) + w(kc - 1)*xh(k)
            x(kc) = w(k - 1)*xh(k) - w(kc - 1)*xh(kc)
        end do
        if (modn .eq. 0) x(ns2 + 1) = w(ns2)*xh(ns2 + 1)
        call dfftf_qp(n, x, xh)
        do i = 3, n, 2
            xim1 = x(i - 1) - x(i)
            x(i) = x(i - 1) + x(i)
            x(i - 1) = xim1
        end do
        return
    end subroutine

