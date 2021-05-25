
    subroutine radfg_sp (ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension ch(ido, l1, ip), cc(ido, ip, l1), &
            c1(ido, l1, ip), c2(idl1, ip), &
            ch2(idl1, ip), wa(*)
        data tpi/6.28318530717958647692d0/
        arg = tpi/float(ip)
        dcp = cos(arg)
        dsp = sin(arg)
        ipph = (ip + 1)/2
        ipp2 = ip + 2
        idp2 = ido + 2
        nbd = (ido - 1)/2
        if (ido .eq. 1) go to 119
        do ik = 1, idl1
            ch2(ik, 1) = c2(ik, 1)
        end do
        do j = 2, ip
            do k = 1, l1
                ch(1, k, j) = c1(1, k, j)
            end do
        end do
        if (nbd .gt. l1) go to 107
        is = -ido
        do j = 2, ip
            is = is + ido
            idij = is
            do i = 3, ido, 2
                idij = idij + 2
                do k = 1, l1
                    ch(i - 1, k, j) = wa(idij - 1)*c1(i - 1, k, j) + wa(idij)*c1(i, k, j)
                    ch(i, k, j) = wa(idij - 1)*c1(i, k, j) - wa(idij)*c1(i - 1, k, j)
                end do
            end do
        end do
        go to 111
107     is = -ido
        do j = 2, ip
            is = is + ido
            do k = 1, l1
                idij = is
                do i = 3, ido, 2
                    idij = idij + 2
                    ch(i - 1, k, j) = wa(idij - 1)*c1(i - 1, k, j) + wa(idij)*c1(i, k, j)
                    ch(i, k, j) = wa(idij - 1)*c1(i, k, j) - wa(idij)*c1(i - 1, k, j)
                end do
            end do
        end do
111     if (nbd .lt. l1) go to 115
        do j = 2, ipph
            jc = ipp2 - j
            do k = 1, l1
                do i = 3, ido, 2
                    c1(i - 1, k, j) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    c1(i - 1, k, jc) = ch(i, k, j) - ch(i, k, jc)
                    c1(i, k, j) = ch(i, k, j) + ch(i, k, jc)
                    c1(i, k, jc) = ch(i - 1, k, jc) - ch(i - 1, k, j)
                end do
            end do
        end do
        go to 121
115     do j = 2, ipph
            jc = ipp2 - j
            do i = 3, ido, 2
                do k = 1, l1
                    c1(i - 1, k, j) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    c1(i - 1, k, jc) = ch(i, k, j) - ch(i, k, jc)
                    c1(i, k, j) = ch(i, k, j) + ch(i, k, jc)
                    c1(i, k, jc) = ch(i - 1, k, jc) - ch(i - 1, k, j)
                end do
            end do
        end do
        go to 121
119     do ik = 1, idl1
            c2(ik, 1) = ch2(ik, 1)
        end do
121     do j = 2, ipph
            jc = ipp2 - j
            do k = 1, l1
                c1(1, k, j) = ch(1, k, j) + ch(1, k, jc)
                c1(1, k, jc) = ch(1, k, jc) - ch(1, k, j)
            end do
        end do
!
        ar1 = 1.0d0
        ai1 = 0.0d0
        do l = 2, ipph
            lc = ipp2 - l
            ar1h = dcp*ar1 - dsp*ai1
            ai1 = dcp*ai1 + dsp*ar1
            ar1 = ar1h
            do ik = 1, idl1
                ch2(ik, l) = c2(ik, 1) + ar1*c2(ik, 2)
                ch2(ik, lc) = ai1*c2(ik, ip)
            end do
            dc2 = ar1
            ds2 = ai1
            ar2 = ar1
            ai2 = ai1
            do j = 3, ipph
                jc = ipp2 - j
                ar2h = dc2*ar2 - ds2*ai2
                ai2 = dc2*ai2 + ds2*ar2
                ar2 = ar2h
                do ik = 1, idl1
                    ch2(ik, l) = ch2(ik, l) + ar2*c2(ik, j)
                    ch2(ik, lc) = ch2(ik, lc) + ai2*c2(ik, jc)
                end do
            end do
        end do

        do j = 2, ipph
            do ik = 1, idl1
                ch2(ik, 1) = ch2(ik, 1) + c2(ik, j)
            end do
        end do
!
        if (ido .lt. l1) go to 132
        do k = 1, l1
            do i = 1, ido
                cc(i, 1, k) = ch(i, k, 1)
            end do
        end do
        go to 135
132     do i = 1, ido
            do k = 1, l1
                cc(i, 1, k) = ch(i, k, 1)
            end do
        end do
135     do j = 2, ipph
            jc = ipp2 - j
            j2 = j + j
            do k = 1, l1
                cc(ido, j2 - 2, k) = ch(1, k, j)
                cc(1, j2 - 1, k) = ch(1, k, jc)
            end do
        end do
        if (ido .eq. 1) return
        if (nbd .lt. l1) go to 141
        do j = 2, ipph
            jc = ipp2 - j
            j2 = j + j
            do k = 1, l1
                do i = 3, ido, 2
                    ic = idp2 - i
                    cc(i - 1, j2 - 1, k) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    cc(ic - 1, j2 - 2, k) = ch(i - 1, k, j) - ch(i - 1, k, jc)
                    cc(i, j2 - 1, k) = ch(i, k, j) + ch(i, k, jc)
                    cc(ic, j2 - 2, k) = ch(i, k, jc) - ch(i, k, j)
                end do
            end do
        end do
        return
141     do j = 2, ipph
            jc = ipp2 - j
            j2 = j + j
            do i = 3, ido, 2
                ic = idp2 - i
                do k = 1, l1
                    cc(i - 1, j2 - 1, k) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    cc(ic - 1, j2 - 2, k) = ch(i - 1, k, j) - ch(i - 1, k, jc)
                    cc(i, j2 - 1, k) = ch(i, k, j) + ch(i, k, jc)
                    cc(ic, j2 - 2, k) = ch(i, k, jc) - ch(i, k, j)
                end do
            end do
        end do
        return
    end subroutine

    subroutine radfg_dp (ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension ch(ido, l1, ip), cc(ido, ip, l1), &
            c1(ido, l1, ip), c2(idl1, ip), &
            ch2(idl1, ip), wa(*)
        data tpi/6.28318530717958647692d0/
        arg = tpi/float(ip)
        dcp = cos(arg)
        dsp = sin(arg)
        ipph = (ip + 1)/2
        ipp2 = ip + 2
        idp2 = ido + 2
        nbd = (ido - 1)/2
        if (ido .eq. 1) go to 119
        do ik = 1, idl1
            ch2(ik, 1) = c2(ik, 1)
        end do
        do j = 2, ip
            do k = 1, l1
                ch(1, k, j) = c1(1, k, j)
            end do
        end do
        if (nbd .gt. l1) go to 107
        is = -ido
        do j = 2, ip
            is = is + ido
            idij = is
            do i = 3, ido, 2
                idij = idij + 2
                do k = 1, l1
                    ch(i - 1, k, j) = wa(idij - 1)*c1(i - 1, k, j) + wa(idij)*c1(i, k, j)
                    ch(i, k, j) = wa(idij - 1)*c1(i, k, j) - wa(idij)*c1(i - 1, k, j)
                end do
            end do
        end do
        go to 111
107     is = -ido
        do j = 2, ip
            is = is + ido
            do k = 1, l1
                idij = is
                do i = 3, ido, 2
                    idij = idij + 2
                    ch(i - 1, k, j) = wa(idij - 1)*c1(i - 1, k, j) + wa(idij)*c1(i, k, j)
                    ch(i, k, j) = wa(idij - 1)*c1(i, k, j) - wa(idij)*c1(i - 1, k, j)
                end do
            end do
        end do
111     if (nbd .lt. l1) go to 115
        do j = 2, ipph
            jc = ipp2 - j
            do k = 1, l1
                do i = 3, ido, 2
                    c1(i - 1, k, j) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    c1(i - 1, k, jc) = ch(i, k, j) - ch(i, k, jc)
                    c1(i, k, j) = ch(i, k, j) + ch(i, k, jc)
                    c1(i, k, jc) = ch(i - 1, k, jc) - ch(i - 1, k, j)
                end do
            end do
        end do
        go to 121
115     do j = 2, ipph
            jc = ipp2 - j
            do i = 3, ido, 2
                do k = 1, l1
                    c1(i - 1, k, j) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    c1(i - 1, k, jc) = ch(i, k, j) - ch(i, k, jc)
                    c1(i, k, j) = ch(i, k, j) + ch(i, k, jc)
                    c1(i, k, jc) = ch(i - 1, k, jc) - ch(i - 1, k, j)
                end do
            end do
        end do
        go to 121
119     do ik = 1, idl1
            c2(ik, 1) = ch2(ik, 1)
        end do
121     do j = 2, ipph
            jc = ipp2 - j
            do k = 1, l1
                c1(1, k, j) = ch(1, k, j) + ch(1, k, jc)
                c1(1, k, jc) = ch(1, k, jc) - ch(1, k, j)
            end do
        end do
!
        ar1 = 1.0d0
        ai1 = 0.0d0
        do l = 2, ipph
            lc = ipp2 - l
            ar1h = dcp*ar1 - dsp*ai1
            ai1 = dcp*ai1 + dsp*ar1
            ar1 = ar1h
            do ik = 1, idl1
                ch2(ik, l) = c2(ik, 1) + ar1*c2(ik, 2)
                ch2(ik, lc) = ai1*c2(ik, ip)
            end do
            dc2 = ar1
            ds2 = ai1
            ar2 = ar1
            ai2 = ai1
            do j = 3, ipph
                jc = ipp2 - j
                ar2h = dc2*ar2 - ds2*ai2
                ai2 = dc2*ai2 + ds2*ar2
                ar2 = ar2h
                do ik = 1, idl1
                    ch2(ik, l) = ch2(ik, l) + ar2*c2(ik, j)
                    ch2(ik, lc) = ch2(ik, lc) + ai2*c2(ik, jc)
                end do
            end do
        end do

        do j = 2, ipph
            do ik = 1, idl1
                ch2(ik, 1) = ch2(ik, 1) + c2(ik, j)
            end do
        end do
!
        if (ido .lt. l1) go to 132
        do k = 1, l1
            do i = 1, ido
                cc(i, 1, k) = ch(i, k, 1)
            end do
        end do
        go to 135
132     do i = 1, ido
            do k = 1, l1
                cc(i, 1, k) = ch(i, k, 1)
            end do
        end do
135     do j = 2, ipph
            jc = ipp2 - j
            j2 = j + j
            do k = 1, l1
                cc(ido, j2 - 2, k) = ch(1, k, j)
                cc(1, j2 - 1, k) = ch(1, k, jc)
            end do
        end do
        if (ido .eq. 1) return
        if (nbd .lt. l1) go to 141
        do j = 2, ipph
            jc = ipp2 - j
            j2 = j + j
            do k = 1, l1
                do i = 3, ido, 2
                    ic = idp2 - i
                    cc(i - 1, j2 - 1, k) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    cc(ic - 1, j2 - 2, k) = ch(i - 1, k, j) - ch(i - 1, k, jc)
                    cc(i, j2 - 1, k) = ch(i, k, j) + ch(i, k, jc)
                    cc(ic, j2 - 2, k) = ch(i, k, jc) - ch(i, k, j)
                end do
            end do
        end do
        return
141     do j = 2, ipph
            jc = ipp2 - j
            j2 = j + j
            do i = 3, ido, 2
                ic = idp2 - i
                do k = 1, l1
                    cc(i - 1, j2 - 1, k) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    cc(ic - 1, j2 - 2, k) = ch(i - 1, k, j) - ch(i - 1, k, jc)
                    cc(i, j2 - 1, k) = ch(i, k, j) + ch(i, k, jc)
                    cc(ic, j2 - 2, k) = ch(i, k, jc) - ch(i, k, j)
                end do
            end do
        end do
        return
    end subroutine

    subroutine radfg_qp (ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension ch(ido, l1, ip), cc(ido, ip, l1), &
            c1(ido, l1, ip), c2(idl1, ip), &
            ch2(idl1, ip), wa(*)
        data tpi/6.28318530717958647692d0/
        arg = tpi/float(ip)
        dcp = cos(arg)
        dsp = sin(arg)
        ipph = (ip + 1)/2
        ipp2 = ip + 2
        idp2 = ido + 2
        nbd = (ido - 1)/2
        if (ido .eq. 1) go to 119
        do ik = 1, idl1
            ch2(ik, 1) = c2(ik, 1)
        end do
        do j = 2, ip
            do k = 1, l1
                ch(1, k, j) = c1(1, k, j)
            end do
        end do
        if (nbd .gt. l1) go to 107
        is = -ido
        do j = 2, ip
            is = is + ido
            idij = is
            do i = 3, ido, 2
                idij = idij + 2
                do k = 1, l1
                    ch(i - 1, k, j) = wa(idij - 1)*c1(i - 1, k, j) + wa(idij)*c1(i, k, j)
                    ch(i, k, j) = wa(idij - 1)*c1(i, k, j) - wa(idij)*c1(i - 1, k, j)
                end do
            end do
        end do
        go to 111
107     is = -ido
        do j = 2, ip
            is = is + ido
            do k = 1, l1
                idij = is
                do i = 3, ido, 2
                    idij = idij + 2
                    ch(i - 1, k, j) = wa(idij - 1)*c1(i - 1, k, j) + wa(idij)*c1(i, k, j)
                    ch(i, k, j) = wa(idij - 1)*c1(i, k, j) - wa(idij)*c1(i - 1, k, j)
                end do
            end do
        end do
111     if (nbd .lt. l1) go to 115
        do j = 2, ipph
            jc = ipp2 - j
            do k = 1, l1
                do i = 3, ido, 2
                    c1(i - 1, k, j) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    c1(i - 1, k, jc) = ch(i, k, j) - ch(i, k, jc)
                    c1(i, k, j) = ch(i, k, j) + ch(i, k, jc)
                    c1(i, k, jc) = ch(i - 1, k, jc) - ch(i - 1, k, j)
                end do
            end do
        end do
        go to 121
115     do j = 2, ipph
            jc = ipp2 - j
            do i = 3, ido, 2
                do k = 1, l1
                    c1(i - 1, k, j) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    c1(i - 1, k, jc) = ch(i, k, j) - ch(i, k, jc)
                    c1(i, k, j) = ch(i, k, j) + ch(i, k, jc)
                    c1(i, k, jc) = ch(i - 1, k, jc) - ch(i - 1, k, j)
                end do
            end do
        end do
        go to 121
119     do ik = 1, idl1
            c2(ik, 1) = ch2(ik, 1)
        end do
121     do j = 2, ipph
            jc = ipp2 - j
            do k = 1, l1
                c1(1, k, j) = ch(1, k, j) + ch(1, k, jc)
                c1(1, k, jc) = ch(1, k, jc) - ch(1, k, j)
            end do
        end do
!
        ar1 = 1.0d0
        ai1 = 0.0d0
        do l = 2, ipph
            lc = ipp2 - l
            ar1h = dcp*ar1 - dsp*ai1
            ai1 = dcp*ai1 + dsp*ar1
            ar1 = ar1h
            do ik = 1, idl1
                ch2(ik, l) = c2(ik, 1) + ar1*c2(ik, 2)
                ch2(ik, lc) = ai1*c2(ik, ip)
            end do
            dc2 = ar1
            ds2 = ai1
            ar2 = ar1
            ai2 = ai1
            do j = 3, ipph
                jc = ipp2 - j
                ar2h = dc2*ar2 - ds2*ai2
                ai2 = dc2*ai2 + ds2*ar2
                ar2 = ar2h
                do ik = 1, idl1
                    ch2(ik, l) = ch2(ik, l) + ar2*c2(ik, j)
                    ch2(ik, lc) = ch2(ik, lc) + ai2*c2(ik, jc)
                end do
            end do
        end do

        do j = 2, ipph
            do ik = 1, idl1
                ch2(ik, 1) = ch2(ik, 1) + c2(ik, j)
            end do
        end do
!
        if (ido .lt. l1) go to 132
        do k = 1, l1
            do i = 1, ido
                cc(i, 1, k) = ch(i, k, 1)
            end do
        end do
        go to 135
132     do i = 1, ido
            do k = 1, l1
                cc(i, 1, k) = ch(i, k, 1)
            end do
        end do
135     do j = 2, ipph
            jc = ipp2 - j
            j2 = j + j
            do k = 1, l1
                cc(ido, j2 - 2, k) = ch(1, k, j)
                cc(1, j2 - 1, k) = ch(1, k, jc)
            end do
        end do
        if (ido .eq. 1) return
        if (nbd .lt. l1) go to 141
        do j = 2, ipph
            jc = ipp2 - j
            j2 = j + j
            do k = 1, l1
                do i = 3, ido, 2
                    ic = idp2 - i
                    cc(i - 1, j2 - 1, k) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    cc(ic - 1, j2 - 2, k) = ch(i - 1, k, j) - ch(i - 1, k, jc)
                    cc(i, j2 - 1, k) = ch(i, k, j) + ch(i, k, jc)
                    cc(ic, j2 - 2, k) = ch(i, k, jc) - ch(i, k, j)
                end do
            end do
        end do
        return
141     do j = 2, ipph
            jc = ipp2 - j
            j2 = j + j
            do i = 3, ido, 2
                ic = idp2 - i
                do k = 1, l1
                    cc(i - 1, j2 - 1, k) = ch(i - 1, k, j) + ch(i - 1, k, jc)
                    cc(ic - 1, j2 - 2, k) = ch(i - 1, k, j) - ch(i - 1, k, jc)
                    cc(i, j2 - 1, k) = ch(i, k, j) + ch(i, k, jc)
                    cc(ic, j2 - 2, k) = ch(i, k, jc) - ch(i, k, j)
                end do
            end do
        end do
        return
    end subroutine

