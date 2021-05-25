
    subroutine zffti_sp (n, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension wsave(*)
        if (n .eq. 1) return
        iw1 = n + n + 1
        iw2 = iw1 + n + n
        call cffti1_sp (n, wsave(iw1), wsave(iw2))
        return
    end subroutine

    subroutine zffti_dp (n, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension wsave(*)
        if (n .eq. 1) return
        iw1 = n + n + 1
        iw2 = iw1 + n + n
        call cffti1_dp (n, wsave(iw1), wsave(iw2))
        return
    end subroutine

    subroutine zffti_qp (n, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension wsave(*)
        if (n .eq. 1) return
        iw1 = n + n + 1
        iw2 = iw1 + n + n
        call cffti1_qp (n, wsave(iw1), wsave(iw2))
        return
    end subroutine


