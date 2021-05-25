
    subroutine cosqb1_sp (n, x, w, xh)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension x(*), w(*), xh(*)
        ns2 = (n + 1)/2
        np2 = n + 2
        do i = 3, n, 2
            xim1 = x(i - 1) + x(i)
            x(i) = x(i) - x(i - 1)
            x(i - 1) = xim1
        end do
        x(1) = x(1) + x(1)
        modn = mod(n, 2)
        if (modn .eq. 0) x(n) = x(n) + x(n)
        call dfftb_sp(n, x, xh)
        do k = 2, ns2
            kc = np2 - k
            xh(k) = w(k - 1)*x(kc) + w(kc - 1)*x(k)
            xh(kc) = w(k - 1)*x(k) - w(kc - 1)*x(kc)
        end do
        if (modn .eq. 0) x(ns2 + 1) = w(ns2)*(x(ns2 + 1) + x(ns2 + 1))
        do k = 2, ns2
            kc = np2 - k
            x(k) = xh(k) + xh(kc)
            x(kc) = xh(k) - xh(kc)
        end do
        x(1) = x(1) + x(1)
        return
    end subroutine

    subroutine cosqb1_dp (n, x, w, xh)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension x(*), w(*), xh(*)
        ns2 = (n + 1)/2
        np2 = n + 2
        do i = 3, n, 2
            xim1 = x(i - 1) + x(i)
            x(i) = x(i) - x(i - 1)
            x(i - 1) = xim1
        end do
        x(1) = x(1) + x(1)
        modn = mod(n, 2)
        if (modn .eq. 0) x(n) = x(n) + x(n)
        call dfftb_dp(n, x, xh)
        do k = 2, ns2
            kc = np2 - k
            xh(k) = w(k - 1)*x(kc) + w(kc - 1)*x(k)
            xh(kc) = w(k - 1)*x(k) - w(kc - 1)*x(kc)
        end do
        if (modn .eq. 0) x(ns2 + 1) = w(ns2)*(x(ns2 + 1) + x(ns2 + 1))
        do k = 2, ns2
            kc = np2 - k
            x(k) = xh(k) + xh(kc)
            x(kc) = xh(k) - xh(kc)
        end do
        x(1) = x(1) + x(1)
        return
    end subroutine

    subroutine cosqb1_qp (n, x, w, xh)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension x(*), w(*), xh(*)
        ns2 = (n + 1)/2
        np2 = n + 2
        do i = 3, n, 2
            xim1 = x(i - 1) + x(i)
            x(i) = x(i) - x(i - 1)
            x(i - 1) = xim1
        end do
        x(1) = x(1) + x(1)
        modn = mod(n, 2)
        if (modn .eq. 0) x(n) = x(n) + x(n)
        call dfftb_qp(n, x, xh)
        do k = 2, ns2
            kc = np2 - k
            xh(k) = w(k - 1)*x(kc) + w(kc - 1)*x(k)
            xh(kc) = w(k - 1)*x(k) - w(kc - 1)*x(kc)
        end do
        if (modn .eq. 0) x(ns2 + 1) = w(ns2)*(x(ns2 + 1) + x(ns2 + 1))
        do k = 2, ns2
            kc = np2 - k
            x(k) = xh(k) + xh(kc)
            x(kc) = xh(k) - xh(kc)
        end do
        x(1) = x(1) + x(1)
        return
    end subroutine

