
    subroutine dsinqb_sp (n, x, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension x(*), wsave(*)
        if (n .gt. 1) go to 101
        x(1) = 4.0d0*x(1)
        return
101     ns2 = n/2
        do k = 2, n, 2
            x(k) = -x(k)
        end do
        call dcosqb_sp (n, x, wsave)
        do k = 1, ns2
            kc = n - k
            xhold = x(k)
            x(k) = x(kc + 1)
            x(kc + 1) = xhold
        end do
        return
    end subroutine

    subroutine dsinqb_dp (n, x, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension x(*), wsave(*)
        if (n .gt. 1) go to 101
        x(1) = 4.0d0*x(1)
        return
101     ns2 = n/2
        do k = 2, n, 2
            x(k) = -x(k)
        end do
        call dcosqb_dp (n, x, wsave)
        do k = 1, ns2
            kc = n - k
            xhold = x(k)
            x(k) = x(kc + 1)
            x(kc + 1) = xhold
        end do
        return
    end subroutine

    subroutine dsinqb_qp (n, x, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension x(*), wsave(*)
        if (n .gt. 1) go to 101
        x(1) = 4.0d0*x(1)
        return
101     ns2 = n/2
        do k = 2, n, 2
            x(k) = -x(k)
        end do
        call dcosqb_qp (n, x, wsave)
        do k = 1, ns2
            kc = n - k
            xhold = x(k)
            x(k) = x(kc + 1)
            x(kc + 1) = xhold
        end do
        return
    end subroutine

