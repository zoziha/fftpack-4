
    subroutine dsinti_sp (n, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension wsave(*)
        data pi/3.14159265358979323846d0/
        if (n .le. 1) return
        ns2 = n/2
        np1 = n + 1
        dt = pi/float(np1)
        do k = 1, ns2
            wsave(k) = 2.0d0*sin(k*dt)
        end do
        call dffti_sp (np1, wsave(ns2 + 1))
        return
    end subroutine

    subroutine dsinti_dp (n, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension wsave(*)
        data pi/3.14159265358979323846d0/
        if (n .le. 1) return
        ns2 = n/2
        np1 = n + 1
        dt = pi/float(np1)
        do k = 1, ns2
            wsave(k) = 2.0d0*sin(k*dt)
        end do
        call dffti_dp (np1, wsave(ns2 + 1))
        return
    end subroutine

    subroutine dsinti_qp (n, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension wsave(*)
        data pi/3.14159265358979323846d0/
        if (n .le. 1) return
        ns2 = n/2
        np1 = n + 1
        dt = pi/float(np1)
        do k = 1, ns2
            wsave(k) = 2.0d0*sin(k*dt)
        end do
        call dffti_qp (np1, wsave(ns2 + 1))
        return
    end subroutine

