
    subroutine dzfftb_sp (n, r, azero, a, b, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension r(*), a(*), b(*), wsave(*)
        if (n - 2) 101, 102, 103
101     r(1) = azero
        return
102     r(1) = azero + a(1)
        r(2) = azero - a(1)
        return
103     ns2 = (n - 1)/2
        do i = 1, ns2
            r(2*i) = 0.5d0*a(i)
            r(2*i + 1) = -0.5d0*b(i)
        end do
        r(1) = azero
        if (mod(n, 2) .eq. 0) r(n) = a(ns2 + 1)
        call dfftb_sp (n, r, wsave(n + 1))
        return
    end subroutine

    subroutine dzfftb_dp (n, r, azero, a, b, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension r(*), a(*), b(*), wsave(*)
        if (n - 2) 101, 102, 103
101     r(1) = azero
        return
102     r(1) = azero + a(1)
        r(2) = azero - a(1)
        return
103     ns2 = (n - 1)/2
        do i = 1, ns2
            r(2*i) = 0.5d0*a(i)
            r(2*i + 1) = -0.5d0*b(i)
        end do
        r(1) = azero
        if (mod(n, 2) .eq. 0) r(n) = a(ns2 + 1)
        call dfftb_dp (n, r, wsave(n + 1))
        return
    end subroutine

    subroutine dzfftb_qp (n, r, azero, a, b, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension r(*), a(*), b(*), wsave(*)
        if (n - 2) 101, 102, 103
101     r(1) = azero
        return
102     r(1) = azero + a(1)
        r(2) = azero - a(1)
        return
103     ns2 = (n - 1)/2
        do i = 1, ns2
            r(2*i) = 0.5d0*a(i)
            r(2*i + 1) = -0.5d0*b(i)
        end do
        r(1) = azero
        if (mod(n, 2) .eq. 0) r(n) = a(ns2 + 1)
        call dfftb_qp (n, r, wsave(n + 1))
        return
    end subroutine

