
    subroutine dsinqi_sp (n, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension wsave(*)
        call dcosqi_sp (n, wsave)
        return
    end subroutine

    subroutine dsinqi_dp (n, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension wsave(*)
        call dcosqi_dp (n, wsave)
        return
    end subroutine

    subroutine dsinqi_qp (n, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension wsave(*)
        call dcosqi_qp (n, wsave)
        return
    end subroutine

