
    subroutine cfftf1_sp (n, c, ch, wa, ifac)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension ch(*), c(*), wa(*), ifac(*)
        nf = ifac(2)
        na = 0
        l1 = 1
        iw = 1
        do k1 = 1, nf
            ip = ifac(k1 + 2)
            l2 = ip*l1
            ido = n/l2
            idot = ido + ido
            idl1 = idot*l1
            if (ip .ne. 4) go to 103
            ix2 = iw + idot
            ix3 = ix2 + idot
            if (na .ne. 0) go to 101
            call passf4_sp (idot, l1, c, ch, wa(iw), wa(ix2), wa(ix3))
            go to 102
101         call passf4_sp (idot, l1, ch, c, wa(iw), wa(ix2), wa(ix3))
102         na = 1 - na
            go to 115
103         if (ip .ne. 2) go to 106
            if (na .ne. 0) go to 104
            call passf2_sp (idot, l1, c, ch, wa(iw))
            go to 105
104         call passf2_sp (idot, l1, ch, c, wa(iw))
105         na = 1 - na
            go to 115
106         if (ip .ne. 3) go to 109
            ix2 = iw + idot
            if (na .ne. 0) go to 107
            call passf3_sp (idot, l1, c, ch, wa(iw), wa(ix2))
            go to 108
107         call passf3_sp (idot, l1, ch, c, wa(iw), wa(ix2))
108         na = 1 - na
            go to 115
109         if (ip .ne. 5) go to 112
            ix2 = iw + idot
            ix3 = ix2 + idot
            ix4 = ix3 + idot
            if (na .ne. 0) go to 110
            call passf5_sp (idot, l1, c, ch, wa(iw), wa(ix2), wa(ix3), wa(ix4))
            go to 111
110         call passf5_sp (idot, l1, ch, c, wa(iw), wa(ix2), wa(ix3), wa(ix4))
111         na = 1 - na
            go to 115
112         if (na .ne. 0) go to 113
            call passf_sp (nac, idot, ip, l1, idl1, c, c, c, ch, ch, wa(iw))
            go to 114
113         call passf_sp (nac, idot, ip, l1, idl1, ch, ch, ch, c, c, wa(iw))
114         if (nac .ne. 0) na = 1 - na
115         l1 = l2
            iw = iw + (ip - 1)*idot
        end do
        if (na .eq. 0) return
        n2 = n + n
        do i = 1, n2
            c(i) = ch(i)
        end do
        return
    end subroutine
    subroutine cfftf1_dp (n, c, ch, wa, ifac)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension ch(*), c(*), wa(*), ifac(*)
        nf = ifac(2)
        na = 0
        l1 = 1
        iw = 1
        do k1 = 1, nf
            ip = ifac(k1 + 2)
            l2 = ip*l1
            ido = n/l2
            idot = ido + ido
            idl1 = idot*l1
            if (ip .ne. 4) go to 103
            ix2 = iw + idot
            ix3 = ix2 + idot
            if (na .ne. 0) go to 101
            call passf4_dp (idot, l1, c, ch, wa(iw), wa(ix2), wa(ix3))
            go to 102
101         call passf4_dp (idot, l1, ch, c, wa(iw), wa(ix2), wa(ix3))
102         na = 1 - na
            go to 115
103         if (ip .ne. 2) go to 106
            if (na .ne. 0) go to 104
            call passf2_dp (idot, l1, c, ch, wa(iw))
            go to 105
104         call passf2_dp (idot, l1, ch, c, wa(iw))
105         na = 1 - na
            go to 115
106         if (ip .ne. 3) go to 109
            ix2 = iw + idot
            if (na .ne. 0) go to 107
            call passf3_dp (idot, l1, c, ch, wa(iw), wa(ix2))
            go to 108
107         call passf3_dp (idot, l1, ch, c, wa(iw), wa(ix2))
108         na = 1 - na
            go to 115
109         if (ip .ne. 5) go to 112
            ix2 = iw + idot
            ix3 = ix2 + idot
            ix4 = ix3 + idot
            if (na .ne. 0) go to 110
            call passf5_dp (idot, l1, c, ch, wa(iw), wa(ix2), wa(ix3), wa(ix4))
            go to 111
110         call passf5_dp (idot, l1, ch, c, wa(iw), wa(ix2), wa(ix3), wa(ix4))
111         na = 1 - na
            go to 115
112         if (na .ne. 0) go to 113
            call passf_dp (nac, idot, ip, l1, idl1, c, c, c, ch, ch, wa(iw))
            go to 114
113         call passf_dp (nac, idot, ip, l1, idl1, ch, ch, ch, c, c, wa(iw))
114         if (nac .ne. 0) na = 1 - na
115         l1 = l2
            iw = iw + (ip - 1)*idot
        end do
        if (na .eq. 0) return
        n2 = n + n
        do i = 1, n2
            c(i) = ch(i)
        end do
        return
    end subroutine
    subroutine cfftf1_qp (n, c, ch, wa, ifac)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension ch(*), c(*), wa(*), ifac(*)
        nf = ifac(2)
        na = 0
        l1 = 1
        iw = 1
        do k1 = 1, nf
            ip = ifac(k1 + 2)
            l2 = ip*l1
            ido = n/l2
            idot = ido + ido
            idl1 = idot*l1
            if (ip .ne. 4) go to 103
            ix2 = iw + idot
            ix3 = ix2 + idot
            if (na .ne. 0) go to 101
            call passf4_qp (idot, l1, c, ch, wa(iw), wa(ix2), wa(ix3))
            go to 102
101         call passf4_qp (idot, l1, ch, c, wa(iw), wa(ix2), wa(ix3))
102         na = 1 - na
            go to 115
103         if (ip .ne. 2) go to 106
            if (na .ne. 0) go to 104
            call passf2_qp (idot, l1, c, ch, wa(iw))
            go to 105
104         call passf2_qp (idot, l1, ch, c, wa(iw))
105         na = 1 - na
            go to 115
106         if (ip .ne. 3) go to 109
            ix2 = iw + idot
            if (na .ne. 0) go to 107
            call passf3_qp (idot, l1, c, ch, wa(iw), wa(ix2))
            go to 108
107         call passf3_qp (idot, l1, ch, c, wa(iw), wa(ix2))
108         na = 1 - na
            go to 115
109         if (ip .ne. 5) go to 112
            ix2 = iw + idot
            ix3 = ix2 + idot
            ix4 = ix3 + idot
            if (na .ne. 0) go to 110
            call passf5_qp (idot, l1, c, ch, wa(iw), wa(ix2), wa(ix3), wa(ix4))
            go to 111
110         call passf5_qp (idot, l1, ch, c, wa(iw), wa(ix2), wa(ix3), wa(ix4))
111         na = 1 - na
            go to 115
112         if (na .ne. 0) go to 113
            call passf_qp (nac, idot, ip, l1, idl1, c, c, c, ch, ch, wa(iw))
            go to 114
113         call passf_qp (nac, idot, ip, l1, idl1, ch, ch, ch, c, c, wa(iw))
114         if (nac .ne. 0) na = 1 - na
115         l1 = l2
            iw = iw + (ip - 1)*idot
        end do
        if (na .eq. 0) return
        n2 = n + n
        do i = 1, n2
            c(i) = ch(i)
        end do
        return
    end subroutine