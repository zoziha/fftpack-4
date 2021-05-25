
    subroutine sint1_sp (n, war, was, xh, x, ifac)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension war(*), was(*), x(*), xh(*), ifac(*)
        data sqrt3/1.73205080756887729352d0/
        do i = 1, n
            xh(i) = war(i)
            war(i) = x(i)
        end do
        if (n - 2) 101, 102, 103
101     xh(1) = xh(1) + xh(1)
        go to 106
102     xhold = sqrt3*(xh(1) + xh(2))
        xh(2) = sqrt3*(xh(1) - xh(2))
        xh(1) = xhold
        go to 106
103     np1 = n + 1
        ns2 = n/2
        x(1) = 0.0d0
        do k = 1, ns2
            kc = np1 - k
            t1 = xh(k) - xh(kc)
            t2 = was(k)*(xh(k) + xh(kc))
            x(k + 1) = t1 + t2
            x(kc + 1) = t2 - t1
        end do
        modn = mod(n, 2)
        if (modn .ne. 0) x(ns2 + 2) = 4.0d0*xh(ns2 + 1)
        call rfftf1_sp (np1, x, xh, war, ifac)
        xh(1) = 0.5d0*x(1)
        do i = 3, n, 2
            xh(i - 1) = -x(i)
            xh(i) = xh(i - 2) + x(i - 1)
        end do
        if (modn .ne. 0) go to 106
        xh(n) = -x(n + 1)
106     do i = 1, n
            x(i) = war(i)
            war(i) = xh(i)
        end do
        return
    end subroutine

    subroutine sint1_dp (n, war, was, xh, x, ifac)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension war(*), was(*), x(*), xh(*), ifac(*)
        data sqrt3/1.73205080756887729352d0/
        do i = 1, n
            xh(i) = war(i)
            war(i) = x(i)
        end do
        if (n - 2) 101, 102, 103
101     xh(1) = xh(1) + xh(1)
        go to 106
102     xhold = sqrt3*(xh(1) + xh(2))
        xh(2) = sqrt3*(xh(1) - xh(2))
        xh(1) = xhold
        go to 106
103     np1 = n + 1
        ns2 = n/2
        x(1) = 0.0d0
        do k = 1, ns2
            kc = np1 - k
            t1 = xh(k) - xh(kc)
            t2 = was(k)*(xh(k) + xh(kc))
            x(k + 1) = t1 + t2
            x(kc + 1) = t2 - t1
        end do
        modn = mod(n, 2)
        if (modn .ne. 0) x(ns2 + 2) = 4.0d0*xh(ns2 + 1)
        call rfftf1_dp (np1, x, xh, war, ifac)
        xh(1) = 0.5d0*x(1)
        do i = 3, n, 2
            xh(i - 1) = -x(i)
            xh(i) = xh(i - 2) + x(i - 1)
        end do
        if (modn .ne. 0) go to 106
        xh(n) = -x(n + 1)
106     do i = 1, n
            x(i) = war(i)
            war(i) = xh(i)
        end do
        return
    end subroutine

    subroutine sint1_qp (n, war, was, xh, x, ifac)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension war(*), was(*), x(*), xh(*), ifac(*)
        data sqrt3/1.73205080756887729352d0/
        do i = 1, n
            xh(i) = war(i)
            war(i) = x(i)
        end do
        if (n - 2) 101, 102, 103
101     xh(1) = xh(1) + xh(1)
        go to 106
102     xhold = sqrt3*(xh(1) + xh(2))
        xh(2) = sqrt3*(xh(1) - xh(2))
        xh(1) = xhold
        go to 106
103     np1 = n + 1
        ns2 = n/2
        x(1) = 0.0d0
        do k = 1, ns2
            kc = np1 - k
            t1 = xh(k) - xh(kc)
            t2 = was(k)*(xh(k) + xh(kc))
            x(k + 1) = t1 + t2
            x(kc + 1) = t2 - t1
        end do
        modn = mod(n, 2)
        if (modn .ne. 0) x(ns2 + 2) = 4.0d0*xh(ns2 + 1)
        call rfftf1_qp (np1, x, xh, war, ifac)
        xh(1) = 0.5d0*x(1)
        do i = 3, n, 2
            xh(i - 1) = -x(i)
            xh(i) = xh(i - 2) + x(i - 1)
        end do
        if (modn .ne. 0) go to 106
        xh(n) = -x(n + 1)
106     do i = 1, n
            x(i) = war(i)
            war(i) = xh(i)
        end do
        return
    end subroutine

