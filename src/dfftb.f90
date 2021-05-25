
    subroutine dfftb_sp (n, r, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension r(*), wsave(*)
        if (n .eq. 1) return
        call rfftb1_sp (n, r, wsave, wsave(n + 1), wsave(2*n + 1))
        return
    end subroutine

    subroutine dfftb_dp (n, r, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension r(*), wsave(*)
        if (n .eq. 1) return
        call rfftb1_dp (n, r, wsave, wsave(n + 1), wsave(2*n + 1))
        return
    end subroutine

    subroutine dfftb_qp (n, r, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension r(*), wsave(*)
        if (n .eq. 1) return
        call rfftb1_qp (n, r, wsave, wsave(n + 1), wsave(2*n + 1))
        return
    end subroutine

