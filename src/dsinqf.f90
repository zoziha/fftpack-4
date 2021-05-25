
    subroutine dsinqf_sp (n, x, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension x(*), wsave(*)
        if (n .eq. 1) return
        ns2 = n/2
        do k = 1, ns2
            kc = n - k
            xhold = x(k)
            x(k) = x(kc + 1)
            x(kc + 1) = xhold
        end do
        call dcosqf_sp (n, x, wsave)
        do k = 2, n, 2
            x(k) = -x(k)
        end do
        return
    end subroutine

    subroutine dsinqf_dp (n, x, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension x(*), wsave(*)
        if (n .eq. 1) return
        ns2 = n/2
        do k = 1, ns2
            kc = n - k
            xhold = x(k)
            x(k) = x(kc + 1)
            x(kc + 1) = xhold
        end do
        call dcosqf_dp (n, x, wsave)
        do k = 2, n, 2
            x(k) = -x(k)
        end do
        return
    end subroutine

    subroutine dsinqf_qp (n, x, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension x(*), wsave(*)
        if (n .eq. 1) return
        ns2 = n/2
        do k = 1, ns2
            kc = n - k
            xhold = x(k)
            x(k) = x(kc + 1)
            x(kc + 1) = xhold
        end do
        call dcosqf_qp (n, x, wsave)
        do k = 2, n, 2
            x(k) = -x(k)
        end do
        return
    end subroutine

