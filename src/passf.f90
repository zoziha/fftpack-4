
    subroutine passf_sp (nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
        use fftpack_kinds, only: sp
        implicit real(sp) (a - h, o - z)
        dimension ch(ido, l1, ip), cc(ido, ip, l1), &
            c1(ido, l1, ip), wa(*), c2(idl1, ip), &
            ch2(idl1, ip)
        idot = ido/2
        nt = ip*idl1
        ipp2 = ip + 2
        ipph = (ip + 1)/2
        idp = ip*ido
!
        if (ido .lt. l1) go to 106
        do j = 2, ipph
            jc = ipp2 - j
            do k = 1, l1
                do i = 1, ido
                    ch(i, k, j) = cc(i, j, k) + cc(i, jc, k)
                    ch(i, k, jc) = cc(i, j, k) - cc(i, jc, k)
                end do
            end do
        end do
        do k = 1, l1
            do i = 1, ido
                ch(i, k, 1) = cc(i, 1, k)
            end do
        end do
        go to 112
106     do j = 2, ipph
            jc = ipp2 - j
            do i = 1, ido
                do k = 1, l1
                    ch(i, k, j) = cc(i, j, k) + cc(i, jc, k)
                    ch(i, k, jc) = cc(i, j, k) - cc(i, jc, k)
                end do
            end do
        end do
        do i = 1, ido
            do k = 1, l1
                ch(i, k, 1) = cc(i, 1, k)
            end do
        end do
112     idl = 2 - ido
        inc = 0
        do l = 2, ipph
            lc = ipp2 - l
            idl = idl + ido
            do ik = 1, idl1
                c2(ik, l) = ch2(ik, 1) + wa(idl - 1)*ch2(ik, 2)
                c2(ik, lc) = -wa(idl)*ch2(ik, ip)
            end do
            idlj = idl
            inc = inc + ido
            do j = 3, ipph
                jc = ipp2 - j
                idlj = idlj + inc
                if (idlj .gt. idp) idlj = idlj - idp
                war = wa(idlj - 1)
                wai = wa(idlj)
                do ik = 1, idl1
                    c2(ik, l) = c2(ik, l) + war*ch2(ik, j)
                    c2(ik, lc) = c2(ik, lc) - wai*ch2(ik, jc)
                end do
            end do
        end do
        do j = 2, ipph
            do ik = 1, idl1
                ch2(ik, 1) = ch2(ik, 1) + ch2(ik, j)
            end do
        end do
        do j = 2, ipph
            jc = ipp2 - j
            do ik = 2, idl1, 2
                ch2(ik - 1, j) = c2(ik - 1, j) - c2(ik, jc)
                ch2(ik - 1, jc) = c2(ik - 1, j) + c2(ik, jc)
                ch2(ik, j) = c2(ik, j) + c2(ik - 1, jc)
                ch2(ik, jc) = c2(ik, j) - c2(ik - 1, jc)
            end do
        end do
        nac = 1
        if (ido .eq. 2) return
        nac = 0
        do ik = 1, idl1
            c2(ik, 1) = ch2(ik, 1)
        end do
        do j = 2, ip
            do k = 1, l1
                c1(1, k, j) = ch(1, k, j)
                c1(2, k, j) = ch(2, k, j)
            end do
        end do
        if (idot .gt. l1) go to 127
        idij = 0
        do j = 2, ip
            idij = idij + 2
            do i = 4, ido, 2
                idij = idij + 2
                do k = 1, l1
                    c1(i - 1, k, j) = wa(idij - 1)*ch(i - 1, k, j) + wa(idij)*ch(i, k, j)
                    c1(i, k, j) = wa(idij - 1)*ch(i, k, j) - wa(idij)*ch(i - 1, k, j)
                end do
            end do
        end do
        return
127     idj = 2 - ido
        do j = 2, ip
            idj = idj + ido
            do k = 1, l1
                idij = idj
                do i = 4, ido, 2
                    idij = idij + 2
                    c1(i - 1, k, j) = wa(idij - 1)*ch(i - 1, k, j) + wa(idij)*ch(i, k, j)
                    c1(i, k, j) = wa(idij - 1)*ch(i, k, j) - wa(idij)*ch(i - 1, k, j)
                end do
            end do
        end do
        return
    end subroutine

    subroutine passf_dp (nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
        use fftpack_kinds, only: dp
        implicit real(dp) (a - h, o - z)
        dimension ch(ido, l1, ip), cc(ido, ip, l1), &
            c1(ido, l1, ip), wa(*), c2(idl1, ip), &
            ch2(idl1, ip)
        idot = ido/2
        nt = ip*idl1
        ipp2 = ip + 2
        ipph = (ip + 1)/2
        idp = ip*ido
!
        if (ido .lt. l1) go to 106
        do j = 2, ipph
            jc = ipp2 - j
            do k = 1, l1
                do i = 1, ido
                    ch(i, k, j) = cc(i, j, k) + cc(i, jc, k)
                    ch(i, k, jc) = cc(i, j, k) - cc(i, jc, k)
                end do
            end do
        end do
        do k = 1, l1
            do i = 1, ido
                ch(i, k, 1) = cc(i, 1, k)
            end do
        end do
        go to 112
106     do j = 2, ipph
            jc = ipp2 - j
            do i = 1, ido
                do k = 1, l1
                    ch(i, k, j) = cc(i, j, k) + cc(i, jc, k)
                    ch(i, k, jc) = cc(i, j, k) - cc(i, jc, k)
                end do
            end do
        end do
        do i = 1, ido
            do k = 1, l1
                ch(i, k, 1) = cc(i, 1, k)
            end do
        end do
112     idl = 2 - ido
        inc = 0
        do l = 2, ipph
            lc = ipp2 - l
            idl = idl + ido
            do ik = 1, idl1
                c2(ik, l) = ch2(ik, 1) + wa(idl - 1)*ch2(ik, 2)
                c2(ik, lc) = -wa(idl)*ch2(ik, ip)
            end do
            idlj = idl
            inc = inc + ido
            do j = 3, ipph
                jc = ipp2 - j
                idlj = idlj + inc
                if (idlj .gt. idp) idlj = idlj - idp
                war = wa(idlj - 1)
                wai = wa(idlj)
                do ik = 1, idl1
                    c2(ik, l) = c2(ik, l) + war*ch2(ik, j)
                    c2(ik, lc) = c2(ik, lc) - wai*ch2(ik, jc)
                end do
            end do
        end do
        do j = 2, ipph
            do ik = 1, idl1
                ch2(ik, 1) = ch2(ik, 1) + ch2(ik, j)
            end do
        end do
        do j = 2, ipph
            jc = ipp2 - j
            do ik = 2, idl1, 2
                ch2(ik - 1, j) = c2(ik - 1, j) - c2(ik, jc)
                ch2(ik - 1, jc) = c2(ik - 1, j) + c2(ik, jc)
                ch2(ik, j) = c2(ik, j) + c2(ik - 1, jc)
                ch2(ik, jc) = c2(ik, j) - c2(ik - 1, jc)
            end do
        end do
        nac = 1
        if (ido .eq. 2) return
        nac = 0
        do ik = 1, idl1
            c2(ik, 1) = ch2(ik, 1)
        end do
        do j = 2, ip
            do k = 1, l1
                c1(1, k, j) = ch(1, k, j)
                c1(2, k, j) = ch(2, k, j)
            end do
        end do
        if (idot .gt. l1) go to 127
        idij = 0
        do j = 2, ip
            idij = idij + 2
            do i = 4, ido, 2
                idij = idij + 2
                do k = 1, l1
                    c1(i - 1, k, j) = wa(idij - 1)*ch(i - 1, k, j) + wa(idij)*ch(i, k, j)
                    c1(i, k, j) = wa(idij - 1)*ch(i, k, j) - wa(idij)*ch(i - 1, k, j)
                end do
            end do
        end do
        return
127     idj = 2 - ido
        do j = 2, ip
            idj = idj + ido
            do k = 1, l1
                idij = idj
                do i = 4, ido, 2
                    idij = idij + 2
                    c1(i - 1, k, j) = wa(idij - 1)*ch(i - 1, k, j) + wa(idij)*ch(i, k, j)
                    c1(i, k, j) = wa(idij - 1)*ch(i, k, j) - wa(idij)*ch(i - 1, k, j)
                end do
            end do
        end do
        return
    end subroutine

    subroutine passf_qp (nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
        use fftpack_kinds, only: qp
        implicit real(qp) (a - h, o - z)
        dimension ch(ido, l1, ip), cc(ido, ip, l1), &
            c1(ido, l1, ip), wa(*), c2(idl1, ip), &
            ch2(idl1, ip)
        idot = ido/2
        nt = ip*idl1
        ipp2 = ip + 2
        ipph = (ip + 1)/2
        idp = ip*ido
!
        if (ido .lt. l1) go to 106
        do j = 2, ipph
            jc = ipp2 - j
            do k = 1, l1
                do i = 1, ido
                    ch(i, k, j) = cc(i, j, k) + cc(i, jc, k)
                    ch(i, k, jc) = cc(i, j, k) - cc(i, jc, k)
                end do
            end do
        end do
        do k = 1, l1
            do i = 1, ido
                ch(i, k, 1) = cc(i, 1, k)
            end do
        end do
        go to 112
106     do j = 2, ipph
            jc = ipp2 - j
            do i = 1, ido
                do k = 1, l1
                    ch(i, k, j) = cc(i, j, k) + cc(i, jc, k)
                    ch(i, k, jc) = cc(i, j, k) - cc(i, jc, k)
                end do
            end do
        end do
        do i = 1, ido
            do k = 1, l1
                ch(i, k, 1) = cc(i, 1, k)
            end do
        end do
112     idl = 2 - ido
        inc = 0
        do l = 2, ipph
            lc = ipp2 - l
            idl = idl + ido
            do ik = 1, idl1
                c2(ik, l) = ch2(ik, 1) + wa(idl - 1)*ch2(ik, 2)
                c2(ik, lc) = -wa(idl)*ch2(ik, ip)
            end do
            idlj = idl
            inc = inc + ido
            do j = 3, ipph
                jc = ipp2 - j
                idlj = idlj + inc
                if (idlj .gt. idp) idlj = idlj - idp
                war = wa(idlj - 1)
                wai = wa(idlj)
                do ik = 1, idl1
                    c2(ik, l) = c2(ik, l) + war*ch2(ik, j)
                    c2(ik, lc) = c2(ik, lc) - wai*ch2(ik, jc)
                end do
            end do
        end do
        do j = 2, ipph
            do ik = 1, idl1
                ch2(ik, 1) = ch2(ik, 1) + ch2(ik, j)
            end do
        end do
        do j = 2, ipph
            jc = ipp2 - j
            do ik = 2, idl1, 2
                ch2(ik - 1, j) = c2(ik - 1, j) - c2(ik, jc)
                ch2(ik - 1, jc) = c2(ik - 1, j) + c2(ik, jc)
                ch2(ik, j) = c2(ik, j) + c2(ik - 1, jc)
                ch2(ik, jc) = c2(ik, j) - c2(ik - 1, jc)
            end do
        end do
        nac = 1
        if (ido .eq. 2) return
        nac = 0
        do ik = 1, idl1
            c2(ik, 1) = ch2(ik, 1)
        end do
        do j = 2, ip
            do k = 1, l1
                c1(1, k, j) = ch(1, k, j)
                c1(2, k, j) = ch(2, k, j)
            end do
        end do
        if (idot .gt. l1) go to 127
        idij = 0
        do j = 2, ip
            idij = idij + 2
            do i = 4, ido, 2
                idij = idij + 2
                do k = 1, l1
                    c1(i - 1, k, j) = wa(idij - 1)*ch(i - 1, k, j) + wa(idij)*ch(i, k, j)
                    c1(i, k, j) = wa(idij - 1)*ch(i, k, j) - wa(idij)*ch(i - 1, k, j)
                end do
            end do
        end do
        return
127     idj = 2 - ido
        do j = 2, ip
            idj = idj + ido
            do k = 1, l1
                idij = idj
                do i = 4, ido, 2
                    idij = idij + 2
                    c1(i - 1, k, j) = wa(idij - 1)*ch(i - 1, k, j) + wa(idij)*ch(i, k, j)
                    c1(i, k, j) = wa(idij - 1)*ch(i, k, j) - wa(idij)*ch(i - 1, k, j)
                end do
            end do
        end do
        return
    end subroutine

