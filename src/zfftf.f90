
    subroutine zfftf_sp (n, c, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension c(*), wsave(*)
        complex(sp) c
        if (n .eq. 1) return
        iw1 = n + n + 1
        iw2 = iw1 + n + n
        call cfftf1_sp (n, c, wsave, wsave(iw1), wsave(iw2))
        return
    end subroutine

    subroutine zfftf_dp (n, c, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension c(*), wsave(*)
        complex(dp) c
        if (n .eq. 1) return
        iw1 = n + n + 1
        iw2 = iw1 + n + n
        call cfftf1_dp (n, c, wsave, wsave(iw1), wsave(iw2))
        return
    end subroutine

    subroutine zfftf_qp (n, c, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension c(*), wsave(*)
        complex(qp) c
        if (n .eq. 1) return
        iw1 = n + n + 1
        iw2 = iw1 + n + n
        call cfftf1_qp (n, c, wsave, wsave(iw1), wsave(iw2))
        return
    end subroutine

