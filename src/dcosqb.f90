
    subroutine dcosqb_sp (n, x, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension x(*), wsave(*)
        data tsqrt2/2.82842712474619009760d0/
        if (n - 2) 101, 102, 103
101     x(1) = 4.0d0*x(1)
        return
102     x1 = 4.0d0*(x(1) + x(2))
        x(2) = tsqrt2*(x(1) - x(2))
        x(1) = x1
        return
103     call cosqb1_sp (n, x, wsave, wsave(n + 1))
        return
    end subroutine

    subroutine dcosqb_dp (n, x, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension x(*), wsave(*)
        data tsqrt2/2.82842712474619009760d0/
        if (n - 2) 101, 102, 103
101     x(1) = 4.0d0*x(1)
        return
102     x1 = 4.0d0*(x(1) + x(2))
        x(2) = tsqrt2*(x(1) - x(2))
        x(1) = x1
        return
103     call cosqb1_dp (n, x, wsave, wsave(n + 1))
        return
    end subroutine

    subroutine dcosqb_qp (n, x, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension x(*), wsave(*)
        data tsqrt2/2.82842712474619009760d0/
        if (n - 2) 101, 102, 103
101     x(1) = 4.0d0*x(1)
        return
102     x1 = 4.0d0*(x(1) + x(2))
        x(2) = tsqrt2*(x(1) - x(2))
        x(1) = x1
        return
103     call cosqb1_qp (n, x, wsave, wsave(n + 1))
        return
    end subroutine

