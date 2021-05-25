
    subroutine rffti1_sp (n, wa, ifac)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension wa(*), ifac(*), ntryh(4)
        data ntryh(1), ntryh(2), ntryh(3), ntryh(4)/4, 2, 3, 5/
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
        is = 0
        nfm1 = nf - 1
        l1 = 1
        if (nfm1 .eq. 0) return
        do k1 = 1, nfm1
            ip = ifac(k1 + 2)
            ld = 0
            l2 = l1*ip
            ido = n/l2
            ipm = ip - 1
            do j = 1, ipm
                ld = ld + l1
                i = is
                argld = float(ld)*argh
                fi = 0.0d0
                do ii = 3, ido, 2
                    i = i + 2
                    fi = fi + 1.0d0
                    arg = fi*argld
                    wa(i - 1) = cos(arg)
                    wa(i) = sin(arg)
                end do
                is = is + ido
            end do
            l1 = l2
        end do
        return
    end subroutine

    subroutine rffti1_dp (n, wa, ifac)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension wa(*), ifac(*), ntryh(4)
        data ntryh(1), ntryh(2), ntryh(3), ntryh(4)/4, 2, 3, 5/
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
        is = 0
        nfm1 = nf - 1
        l1 = 1
        if (nfm1 .eq. 0) return
        do k1 = 1, nfm1
            ip = ifac(k1 + 2)
            ld = 0
            l2 = l1*ip
            ido = n/l2
            ipm = ip - 1
            do j = 1, ipm
                ld = ld + l1
                i = is
                argld = float(ld)*argh
                fi = 0.0d0
                do ii = 3, ido, 2
                    i = i + 2
                    fi = fi + 1.0d0
                    arg = fi*argld
                    wa(i - 1) = cos(arg)
                    wa(i) = sin(arg)
                end do
                is = is + ido
            end do
            l1 = l2
        end do
        return
    end subroutine

    subroutine rffti1_qp (n, wa, ifac)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension wa(*), ifac(*), ntryh(4)
        data ntryh(1), ntryh(2), ntryh(3), ntryh(4)/4, 2, 3, 5/
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
        is = 0
        nfm1 = nf - 1
        l1 = 1
        if (nfm1 .eq. 0) return
        do k1 = 1, nfm1
            ip = ifac(k1 + 2)
            ld = 0
            l2 = l1*ip
            ido = n/l2
            ipm = ip - 1
            do j = 1, ipm
                ld = ld + l1
                i = is
                argld = float(ld)*argh
                fi = 0.0d0
                do ii = 3, ido, 2
                    i = i + 2
                    fi = fi + 1.0d0
                    arg = fi*argld
                    wa(i - 1) = cos(arg)
                    wa(i) = sin(arg)
                end do
                is = is + ido
            end do
            l1 = l2
        end do
        return
    end subroutine

