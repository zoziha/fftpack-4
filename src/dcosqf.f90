
    subroutine dcosqf_sp (n, x, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension x(*), wsave(*)
        data sqrt2/1.41421356237309504880d0/
        if (n - 2) 102, 101, 103
101     tsqx = sqrt2*x(2)
        x(2) = x(1) - tsqx
        x(1) = x(1) + tsqx
102     return
103     call cosqf1_sp (n, x, wsave, wsave(n + 1))
        return
    end subroutine

    subroutine dcosqf_dp (n, x, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension x(*), wsave(*)
        data sqrt2/1.41421356237309504880d0/
        if (n - 2) 102, 101, 103
101     tsqx = sqrt2*x(2)
        x(2) = x(1) - tsqx
        x(1) = x(1) + tsqx
102     return
103     call cosqf1_dp (n, x, wsave, wsave(n + 1))
        return
    end subroutine

    subroutine dcosqf_qp (n, x, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension x(*), wsave(*)
        data sqrt2/1.41421356237309504880d0/
        if (n - 2) 102, 101, 103
101     tsqx = sqrt2*x(2)
        x(2) = x(1) - tsqx
        x(1) = x(1) + tsqx
102     return
103     call cosqf1_qp (n, x, wsave, wsave(n + 1))
        return
    end subroutine

