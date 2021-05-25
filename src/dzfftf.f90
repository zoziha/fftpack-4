
    subroutine dzfftf_sp (n, r, azero, a, b, wsave)
!
!                       version 3  june 1979
!
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension r(*), a(*), b(*), wsave(*)
        if (n - 2) 101, 102, 103
101     azero = r(1)
        return
102     azero = 0.5d0*(r(1) + r(2))
        a(1) = 0.5d0*(r(1) - r(2))
        return
103     do i = 1, n
            wsave(i) = r(i)
        end do
        call dfftf_sp (n, wsave, wsave(n + 1))
        cf = 2.0d0/float(n)
        cfm = -cf
        azero = 0.5d0*cf*wsave(1)
        ns2 = (n + 1)/2
        ns2m = ns2 - 1
        do i = 1, ns2m
            a(i) = cf*wsave(2*i)
            b(i) = cfm*wsave(2*i + 1)
        end do
        if (mod(n, 2) .eq. 1) return
        a(ns2) = 0.5d0*cf*wsave(n)
        b(ns2) = 0.0d0
        return
    end subroutine

    subroutine dzfftf_dp (n, r, azero, a, b, wsave)
!
!                       version 3  june 1979
!
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension r(*), a(*), b(*), wsave(*)
        if (n - 2) 101, 102, 103
101     azero = r(1)
        return
102     azero = 0.5d0*(r(1) + r(2))
        a(1) = 0.5d0*(r(1) - r(2))
        return
103     do i = 1, n
            wsave(i) = r(i)
        end do
        call dfftf_dp (n, wsave, wsave(n + 1))
        cf = 2.0d0/float(n)
        cfm = -cf
        azero = 0.5d0*cf*wsave(1)
        ns2 = (n + 1)/2
        ns2m = ns2 - 1
        do i = 1, ns2m
            a(i) = cf*wsave(2*i)
            b(i) = cfm*wsave(2*i + 1)
        end do
        if (mod(n, 2) .eq. 1) return
        a(ns2) = 0.5d0*cf*wsave(n)
        b(ns2) = 0.0d0
        return
    end subroutine

    subroutine dzfftf_qp (n, r, azero, a, b, wsave)
!
!                       version 3  june 1979
!
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension r(*), a(*), b(*), wsave(*)
        if (n - 2) 101, 102, 103
101     azero = r(1)
        return
102     azero = 0.5d0*(r(1) + r(2))
        a(1) = 0.5d0*(r(1) - r(2))
        return
103     do i = 1, n
            wsave(i) = r(i)
        end do
        call dfftf_qp (n, wsave, wsave(n + 1))
        cf = 2.0d0/float(n)
        cfm = -cf
        azero = 0.5d0*cf*wsave(1)
        ns2 = (n + 1)/2
        ns2m = ns2 - 1
        do i = 1, ns2m
            a(i) = cf*wsave(2*i)
            b(i) = cfm*wsave(2*i + 1)
        end do
        if (mod(n, 2) .eq. 1) return
        a(ns2) = 0.5d0*cf*wsave(n)
        b(ns2) = 0.0d0
        return
    end subroutine

