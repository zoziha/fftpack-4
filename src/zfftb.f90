
    subroutine zfftb_sp (n, c, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension c(*), wsave(*)
        complex(sp) c
            !! tofix
        if (n .eq. 1) return
        iw1 = n + n + 1
        iw2 = iw1 + n + n
        call cfftb1_sp (n, c, wsave, wsave(iw1), wsave(iw2))
        return
    end subroutine

    subroutine zfftb_dp (n, c, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension c(*), wsave(*)
        complex(dp) c
            !! tofix
        if (n .eq. 1) return
        iw1 = n + n + 1
        iw2 = iw1 + n + n
        call cfftb1_dp (n, c, wsave, wsave(iw1), wsave(iw2))
        return
    end subroutine

    subroutine zfftb_qp (n, c, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension c(*), wsave(*)
        complex(qp) c
            !! tofix
        if (n .eq. 1) return
        iw1 = n + n + 1
        iw2 = iw1 + n + n
        call cfftb1_qp (n, c, wsave, wsave(iw1), wsave(iw2))
        return
    end subroutine

