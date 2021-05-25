
    subroutine dcosqi_sp (n, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension wsave(*)
        data pih/1.57079632679489661923d0/
        dt = pih/float(n)
        fk = 0.0d0
        do k = 1, n
            fk = fk + 1.0d0
            wsave(k) = cos(fk*dt)
        end do
        call dffti_sp (n, wsave(n + 1))
        return
    end subroutine

    subroutine dcosqi_dp (n, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension wsave(*)
        data pih/1.57079632679489661923d0/
        dt = pih/float(n)
        fk = 0.0d0
        do k = 1, n
            fk = fk + 1.0d0
            wsave(k) = cos(fk*dt)
        end do
        call dffti_dp (n, wsave(n + 1))
        return
    end subroutine

    subroutine dcosqi_qp (n, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension wsave(*)
        data pih/1.57079632679489661923d0/
        dt = pih/float(n)
        fk = 0.0d0
        do k = 1, n
            fk = fk + 1.0d0
            wsave(k) = cos(fk*dt)
        end do
        call dffti_qp (n, wsave(n + 1))
        return
    end subroutine

