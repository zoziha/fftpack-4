
    subroutine dzffti_sp (n, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension wsave(*)
        if (n .eq. 1) return
        call ezfft1_sp (n, wsave(2*n + 1), wsave(3*n + 1))
        return
    end subroutine

    subroutine dzffti_dp (n, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension wsave(*)
        if (n .eq. 1) return
        call ezfft1_dp (n, wsave(2*n + 1), wsave(3*n + 1))
        return
    end subroutine

    subroutine dzffti_qp (n, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension wsave(*)
        if (n .eq. 1) return
        call ezfft1_qp (n, wsave(2*n + 1), wsave(3*n + 1))
        return
    end subroutine

