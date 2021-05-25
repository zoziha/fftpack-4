
    subroutine dcosti_sp (n, wsave)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension wsave(*)
        data pi/3.14159265358979323846d0/
        if (n .le. 3) return
        nm1 = n - 1
        np1 = n + 1
        ns2 = n/2
        dt = pi/float(nm1)
        fk = 0.0d0
        do k = 2, ns2
            kc = np1 - k
            fk = fk + 1.0d0
            wsave(k) = 2.0d0*sin(fk*dt)
            wsave(kc) = 2.0d0*cos(fk*dt)
        end do
        call dffti_sp (nm1, wsave(n + 1))
        return
    end subroutine

    subroutine dcosti_dp (n, wsave)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension wsave(*)
        data pi/3.14159265358979323846d0/
        if (n .le. 3) return
        nm1 = n - 1
        np1 = n + 1
        ns2 = n/2
        dt = pi/float(nm1)
        fk = 0.0d0
        do k = 2, ns2
            kc = np1 - k
            fk = fk + 1.0d0
            wsave(k) = 2.0d0*sin(fk*dt)
            wsave(kc) = 2.0d0*cos(fk*dt)
        end do
        call dffti_dp (nm1, wsave(n + 1))
        return
    end subroutine

    subroutine dcosti_qp (n, wsave)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension wsave(*)
        data pi/3.14159265358979323846d0/
        if (n .le. 3) return
        nm1 = n - 1
        np1 = n + 1
        ns2 = n/2
        dt = pi/float(nm1)
        fk = 0.0d0
        do k = 2, ns2
            kc = np1 - k
            fk = fk + 1.0d0
            wsave(k) = 2.0d0*sin(fk*dt)
            wsave(kc) = 2.0d0*cos(fk*dt)
        end do
        call dffti_qp (nm1, wsave(n + 1))
        return
    end subroutine

