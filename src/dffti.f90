
    subroutine dffti_sp (n, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension wsave(*)
        if (n .eq. 1) return
        call rffti1_sp (n, wsave(n + 1), wsave(2*n + 1))
        return
    end subroutine

    subroutine dffti_dp (n, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension wsave(*)
        if (n .eq. 1) return
        call rffti1_dp (n, wsave(n + 1), wsave(2*n + 1))
        return
    end subroutine

    subroutine dffti_qp (n, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension wsave(*)
        if (n .eq. 1) return
        call rffti1_qp (n, wsave(n + 1), wsave(2*n + 1))
        return
    end subroutine

