
    subroutine cffti1_sp (n, wa, ifac)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension wa(*), ifac(*), ntryh(4)
        data ntryh(1), ntryh(2), ntryh(3), ntryh(4)/3, 4, 2, 5/
        nl = n
        nf = 0
        j = 0
101     j = j + 1
        if (j - 4) 102, 102, 103
102     ntry = ntryh(j)
        go to 104
103     ntry = ntry + 2
104     nq = nl/ntry
        nr = nl - ntry*nq
        if (nr) 101, 105, 101
105     nf = nf + 1
        ifac(nf + 2) = ntry
        nl = nq
        if (ntry .ne. 2) go to 107
        if (nf .eq. 1) go to 107
        do i = 2, nf
            ib = nf - i + 2
            ifac(ib + 2) = ifac(ib + 1)
        end do
        ifac(3) = 2
107     if (nl .ne. 1) go to 104
        ifac(1) = n
        ifac(2) = nf
        tpi = 6.28318530717958647692d0
        argh = tpi/float(n)
        i = 2
        l1 = 1
        do k1 = 1, nf
            ip = ifac(k1 + 2)
            ld = 0
            l2 = l1*ip
            ido = n/l2
            idot = ido + ido + 2
            ipm = ip - 1
            cycle_do: do j = 1, ipm
                i1 = i
                wa(i - 1) = 1.0d0
                wa(i) = 0.0d0
                ld = ld + l1
                fi = 0.0d0
                argld = float(ld)*argh
                do ii = 4, idot, 2
                    i = i + 2
                    fi = fi + 1.d0
                    arg = fi*argld
                    wa(i - 1) = cos(arg)
                    wa(i) = sin(arg)
                end do
                if (ip .le. 5) cycle cycle_do
                wa(i1 - 1) = wa(i - 1)
                wa(i1) = wa(i)
            end do cycle_do
            l1 = l2
        end do
        return
    end subroutine

    subroutine cffti1_dp (n, wa, ifac)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension wa(*), ifac(*), ntryh(4)
        data ntryh(1), ntryh(2), ntryh(3), ntryh(4)/3, 4, 2, 5/
        nl = n
        nf = 0
        j = 0
101     j = j + 1
        if (j - 4) 102, 102, 103
102     ntry = ntryh(j)
        go to 104
103     ntry = ntry + 2
104     nq = nl/ntry
        nr = nl - ntry*nq
        if (nr) 101, 105, 101
105     nf = nf + 1
        ifac(nf + 2) = ntry
        nl = nq
        if (ntry .ne. 2) go to 107
        if (nf .eq. 1) go to 107
        do i = 2, nf
            ib = nf - i + 2
            ifac(ib + 2) = ifac(ib + 1)
        end do
        ifac(3) = 2
107     if (nl .ne. 1) go to 104
        ifac(1) = n
        ifac(2) = nf
        tpi = 6.28318530717958647692d0
        argh = tpi/float(n)
        i = 2
        l1 = 1
        do k1 = 1, nf
            ip = ifac(k1 + 2)
            ld = 0
            l2 = l1*ip
            ido = n/l2
            idot = ido + ido + 2
            ipm = ip - 1
            cycle_do: do j = 1, ipm
                i1 = i
                wa(i - 1) = 1.0d0
                wa(i) = 0.0d0
                ld = ld + l1
                fi = 0.0d0
                argld = float(ld)*argh
                do ii = 4, idot, 2
                    i = i + 2
                    fi = fi + 1.d0
                    arg = fi*argld
                    wa(i - 1) = cos(arg)
                    wa(i) = sin(arg)
                end do
                if (ip .le. 5) cycle cycle_do
                wa(i1 - 1) = wa(i - 1)
                wa(i1) = wa(i)
            end do cycle_do
            l1 = l2
        end do
        return
    end subroutine

    subroutine cffti1_qp (n, wa, ifac)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension wa(*), ifac(*), ntryh(4)
        data ntryh(1), ntryh(2), ntryh(3), ntryh(4)/3, 4, 2, 5/
        nl = n
        nf = 0
        j = 0
101     j = j + 1
        if (j - 4) 102, 102, 103
102     ntry = ntryh(j)
        go to 104
103     ntry = ntry + 2
104     nq = nl/ntry
        nr = nl - ntry*nq
        if (nr) 101, 105, 101
105     nf = nf + 1
        ifac(nf + 2) = ntry
        nl = nq
        if (ntry .ne. 2) go to 107
        if (nf .eq. 1) go to 107
        do i = 2, nf
            ib = nf - i + 2
            ifac(ib + 2) = ifac(ib + 1)
        end do
        ifac(3) = 2
107     if (nl .ne. 1) go to 104
        ifac(1) = n
        ifac(2) = nf
        tpi = 6.28318530717958647692d0
        argh = tpi/float(n)
        i = 2
        l1 = 1
        do k1 = 1, nf
            ip = ifac(k1 + 2)
            ld = 0
            l2 = l1*ip
            ido = n/l2
            idot = ido + ido + 2
            ipm = ip - 1
            cycle_do: do j = 1, ipm
                i1 = i
                wa(i - 1) = 1.0d0
                wa(i) = 0.0d0
                ld = ld + l1
                fi = 0.0d0
                argld = float(ld)*argh
                do ii = 4, idot, 2
                    i = i + 2
                    fi = fi + 1.d0
                    arg = fi*argld
                    wa(i - 1) = cos(arg)
                    wa(i) = sin(arg)
                end do
                if (ip .le. 5) cycle cycle_do
                wa(i1 - 1) = wa(i - 1)
                wa(i1) = wa(i)
            end do cycle_do
            l1 = l2
        end do
        return
    end subroutine

