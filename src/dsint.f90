
    subroutine dsint_sp (n, x, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension x(*), wsave(*)
        np1 = n + 1
        iw1 = n/2 + 1
        iw2 = iw1 + np1
        iw3 = iw2 + np1
        call sint1_sp (n, x, wsave, wsave(iw1), wsave(iw2), wsave(iw3))
        return
    end subroutine

    subroutine dsint_dp (n, x, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension x(*), wsave(*)
        np1 = n + 1
        iw1 = n/2 + 1
        iw2 = iw1 + np1
        iw3 = iw2 + np1
        call sint1_dp (n, x, wsave, wsave(iw1), wsave(iw2), wsave(iw3))
        return
    end subroutine

    subroutine dsint_qp (n, x, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension x(*), wsave(*)
        np1 = n + 1
        iw1 = n/2 + 1
        iw2 = iw1 + np1
        iw3 = iw2 + np1
        call sint1_qp (n, x, wsave, wsave(iw1), wsave(iw2), wsave(iw3))
        return
    end subroutine

