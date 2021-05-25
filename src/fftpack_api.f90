
module fftpack_api
    use fftpack_kinds, only: sp, dp, qp
    implicit none
    ! private
    ! public :: cfftb1

    interface cfftb1
            subroutine cfftb1_sp (n, c, ch, wa, ifac)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
            subroutine cfftb1_dp (n, c, ch, wa, ifac)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
            subroutine cfftb1_qp (n, c, ch, wa, ifac)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
    end interface cfftb1

    interface cfftf1
            subroutine cfftf1_sp (n, c, ch, wa, ifac)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
            subroutine cfftf1_dp (n, c, ch, wa, ifac)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
            subroutine cfftf1_qp (n, c, ch, wa, ifac)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
    end interface cfftf1

    interface cffti1
            subroutine cffti1_sp (n, wa, ifac)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension wa(*), ifac(*)
            end subroutine
            subroutine cffti1_dp (n, wa, ifac)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension wa(*), ifac(*)
            end subroutine
            subroutine cffti1_qp (n, wa, ifac)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension wa(*), ifac(*)
            end subroutine
    end interface cffti1

    interface cosqb1
            subroutine cosqb1_sp (n, x, w, xh)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension x(1), w(1), xh(1)
            end subroutine
            subroutine cosqb1_dp (n, x, w, xh)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension x(1), w(1), xh(1)
            end subroutine
            subroutine cosqb1_qp (n, x, w, xh)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension x(1), w(1), xh(1)
            end subroutine
    end interface cosqb1

    interface cosqf1
            subroutine cosqf1_sp (n, x, w, xh)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension x(1), w(1), xh(1)
            end subroutine
            subroutine cosqf1_dp (n, x, w, xh)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension x(1), w(1), xh(1)
            end subroutine
            subroutine cosqf1_qp (n, x, w, xh)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension x(1), w(1), xh(1)
            end subroutine
    end interface cosqf1

    interface dcosqb
            subroutine dcosqb_sp (n, x, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension x(*), wsave(*)
            end subroutine
            subroutine dcosqb_dp (n, x, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension x(*), wsave(*)
            end subroutine
            subroutine dcosqb_qp (n, x, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension x(*), wsave(*)
            end subroutine
    end interface dcosqb

    interface dcosqf
            subroutine dcosqf_sp (n, x, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension x(*), wsave(*)
            end subroutine
            subroutine dcosqf_dp (n, x, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension x(*), wsave(*)
            end subroutine
            subroutine dcosqf_qp (n, x, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension x(*), wsave(*)
            end subroutine
    end interface dcosqf

    interface dcosqi
            subroutine dcosqi_sp (n, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dcosqi_dp (n, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dcosqi_qp (n, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
    end interface dcosqi

    interface dcost
            subroutine dcost_sp (n, x, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension x(*), wsave(*)
            end subroutine
            subroutine dcost_dp (n, x, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension x(*), wsave(*)
            end subroutine
            subroutine dcost_qp (n, x, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension x(*), wsave(*)
            end subroutine
    end interface dcost

    interface dcosti
            subroutine dcosti_sp (n, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dcosti_dp (n, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dcosti_qp (n, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
    end interface dcosti

    interface dfftb
            subroutine dfftb_sp (n, r, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension r(1), wsave(1)
            end subroutine
            subroutine dfftb_dp (n, r, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension r(1), wsave(1)
            end subroutine
            subroutine dfftb_qp (n, r, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension r(1), wsave(1)
            end subroutine
    end interface dfftb

    interface dfftf
            subroutine dfftf_sp (n, r, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension r(1), wsave(1)
            end subroutine
            subroutine dfftf_dp (n, r, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension r(1), wsave(1)
            end subroutine
            subroutine dfftf_qp (n, r, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension r(1), wsave(1)
            end subroutine
    end interface dfftf

    interface dffti
            subroutine dffti_sp (n, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dffti_dp (n, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dffti_qp (n, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
    end interface dffti

    interface dsinqb
            subroutine dsinqb_sp (n, x, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension x(1), wsave(1)
            end subroutine
            subroutine dsinqb_dp (n, x, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension x(1), wsave(1)
            end subroutine
            subroutine dsinqb_qp (n, x, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension x(1), wsave(1)
            end subroutine
    end interface dsinqb

    interface dsinqf
            subroutine dsinqf_sp (n, x, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension x(1), wsave(1)
            end subroutine
            subroutine dsinqf_dp (n, x, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension x(1), wsave(1)
            end subroutine
            subroutine dsinqf_qp (n, x, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension x(1), wsave(1)
            end subroutine
    end interface dsinqf

    interface dsinqi
            subroutine dsinqi_sp (n, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dsinqi_dp (n, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dsinqi_qp (n, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
    end interface dsinqi

    interface dsint
            subroutine dsint_sp (n, x, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension x(1), wsave(1)
            end subroutine
            subroutine dsint_dp (n, x, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension x(1), wsave(1)
            end subroutine
            subroutine dsint_qp (n, x, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension x(1), wsave(1)
            end subroutine
    end interface dsint

    interface dsinti
            subroutine dsinti_sp (n, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dsinti_dp (n, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dsinti_qp (n, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
    end interface dsinti

    interface dzfftb
            subroutine dzfftb_sp (n, r, azero, a, b, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension r(*), a(*), b(*), wsave(*)
            end subroutine
            subroutine dzfftb_dp (n, r, azero, a, b, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension r(*), a(*), b(*), wsave(*)
            end subroutine
            subroutine dzfftb_qp (n, r, azero, a, b, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension r(*), a(*), b(*), wsave(*)
            end subroutine
    end interface dzfftb

    interface dzfftf
            subroutine dzfftf_sp (n, r, azero, a, b, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension r(*), a(*), b(*), wsave(*)
            end subroutine
            subroutine dzfftf_dp (n, r, azero, a, b, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension r(*), a(*), b(*), wsave(*)
            end subroutine
            subroutine dzfftf_qp (n, r, azero, a, b, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension r(*), a(*), b(*), wsave(*)
            end subroutine
    end interface dzfftf

    interface dzffti
            subroutine dzffti_sp (n, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dzffti_dp (n, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
            subroutine dzffti_qp (n, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension wsave(1)
            end subroutine
    end interface dzffti

    interface ezfft1
            subroutine ezfft1_sp (n, wa, ifac)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension wa(*), ifac(*), ntryh(4)
            end subroutine
            subroutine ezfft1_dp (n, wa, ifac)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension wa(*), ifac(*), ntryh(4)
            end subroutine
            subroutine ezfft1_qp (n, wa, ifac)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension wa(*), ifac(*), ntryh(4)
            end subroutine
    end interface ezfft1

    interface passb
            subroutine passb_sp (nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), wa(1), c2(idl1, ip), &
                    ch2(idl1, ip)
            end subroutine
            subroutine passb_dp (nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), wa(1), c2(idl1, ip), &
                    ch2(idl1, ip)
            end subroutine
            subroutine passb_qp (nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), wa(1), c2(idl1, ip), &
                    ch2(idl1, ip)
            end subroutine
    end interface passb

    interface passb2
            subroutine passb2_sp (ido, l1, cc, ch, wa1)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 2, l1), ch(ido, l1, 2), &
                    wa1(1)
            end subroutine
            subroutine passb2_dp (ido, l1, cc, ch, wa1)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 2, l1), ch(ido, l1, 2), &
                    wa1(1)
            end subroutine
            subroutine passb2_qp (ido, l1, cc, ch, wa1)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 2, l1), ch(ido, l1, 2), &
                    wa1(1)
            end subroutine
    end interface passb2

    interface passb3
            subroutine passb3_sp (ido, l1, cc, ch, wa1, wa2)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 3, l1), ch(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
            subroutine passb3_dp (ido, l1, cc, ch, wa1, wa2)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 3, l1), ch(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
            subroutine passb3_qp (ido, l1, cc, ch, wa1, wa2)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 3, l1), ch(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
    end interface passb3

    interface passb4
            subroutine passb4_sp (ido, l1, cc, ch, wa1, wa2, wa3)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 4, l1), ch(ido, l1, 4), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
            subroutine passb4_dp (ido, l1, cc, ch, wa1, wa2, wa3)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 4, l1), ch(ido, l1, 4), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
            subroutine passb4_qp (ido, l1, cc, ch, wa1, wa2, wa3)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 4, l1), ch(ido, l1, 4), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
    end interface passb4

    interface passb5
            subroutine passb5_sp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 5, l1), ch(ido, l1, 5), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
            subroutine passb5_dp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 5, l1), ch(ido, l1, 5), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
            subroutine passb5_qp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 5, l1), ch(ido, l1, 5), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
    end interface passb5

    interface passf
            subroutine passf_sp (nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), wa(1), c2(idl1, ip), &
                    ch2(idl1, ip)
            end subroutine
            subroutine passf_dp (nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), wa(1), c2(idl1, ip), &
                    ch2(idl1, ip)
            end subroutine
            subroutine passf_qp (nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), wa(1), c2(idl1, ip), &
                    ch2(idl1, ip)
            end subroutine
    end interface passf

    interface passf2
            subroutine passf2_sp (ido, l1, cc, ch, wa1)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 2, l1), ch(ido, l1, 2), &
                    wa1(1)
            end subroutine
            subroutine passf2_dp (ido, l1, cc, ch, wa1)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 2, l1), ch(ido, l1, 2), &
                    wa1(1)
            end subroutine
            subroutine passf2_qp (ido, l1, cc, ch, wa1)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 2, l1), ch(ido, l1, 2), &
                    wa1(1)
            end subroutine
    end interface passf2

    interface passf3
            subroutine passf3_sp (ido, l1, cc, ch, wa1, wa2)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 3, l1), ch(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
            subroutine passf3_dp (ido, l1, cc, ch, wa1, wa2)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 3, l1), ch(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
            subroutine passf3_qp (ido, l1, cc, ch, wa1, wa2)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 3, l1), ch(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
    end interface passf3

    interface passf4
            subroutine passf4_sp (ido, l1, cc, ch, wa1, wa2, wa3)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 4, l1), ch(ido, l1, 4), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
            subroutine passf4_dp (ido, l1, cc, ch, wa1, wa2, wa3)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 4, l1), ch(ido, l1, 4), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
            subroutine passf4_qp (ido, l1, cc, ch, wa1, wa2, wa3)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 4, l1), ch(ido, l1, 4), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
    end interface passf4

    interface passf5
            subroutine passf5_sp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 5, l1), ch(ido, l1, 5), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
            subroutine passf5_dp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 5, l1), ch(ido, l1, 5), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
            subroutine passf5_qp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 5, l1), ch(ido, l1, 5), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
    end interface passf5

    ! interface radb2
    !     #:for k1, t1 in REAL_KINDS_TYPES
    !         subroutine radb2_qp (n, wsave)
    !             import qp
    !             implicit real(qp) (a - h, o - z)
    !             dimension wsave(*)
    !         end subroutine
    !     #:endfor
    ! end interface radb2

    ! interface radb3
    !     #:for k1, t1 in REAL_KINDS_TYPES
    !         subroutine radb3_qp (n, wsave)
    !             import qp
    !             implicit real(qp) (a - h, o - z)
    !             dimension wsave(*)
    !         end subroutine
    !     #:endfor
    ! end interface radb3

    ! interface radb4
    !     #:for k1, t1 in REAL_KINDS_TYPES
    !         subroutine radb4_qp (n, wsave)
    !             import qp
    !             implicit real(qp) (a - h, o - z)
    !             dimension wsave(*)
    !         end subroutine
    !     #:endfor
    ! end interface radb4

    ! interface radb5
    !     #:for k1, t1 in REAL_KINDS_TYPES
    !         subroutine radb5_qp (n, wsave)
    !             import qp
    !             implicit real(qp) (a - h, o - z)
    !             dimension wsave(*)
    !         end subroutine
    !     #:endfor
    ! end interface radb5

    ! interface radbg
    !     #:for k1, t1 in REAL_KINDS_TYPES
    !         subroutine radbg_qp (n, wsave)
    !             import qp
    !             implicit real(qp) (a - h, o - z)
    !             dimension wsave(*)
    !         end subroutine
    !     #:endfor
    ! end interface radbg

    interface radb2
            subroutine radb2_sp (ido, l1, cc, ch, wa1)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 2, l1), ch(ido, l1, 2), &
                    wa1(1)
            end subroutine
            subroutine radb2_dp (ido, l1, cc, ch, wa1)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 2, l1), ch(ido, l1, 2), &
                    wa1(1)
            end subroutine
            subroutine radb2_qp (ido, l1, cc, ch, wa1)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 2, l1), ch(ido, l1, 2), &
                    wa1(1)
            end subroutine
    end interface radb2

    interface radb3
            subroutine radb3_sp (ido, l1, cc, ch, wa1, wa2)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 3, l1), ch(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
            subroutine radb3_dp (ido, l1, cc, ch, wa1, wa2)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 3, l1), ch(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
            subroutine radb3_qp (ido, l1, cc, ch, wa1, wa2)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 3, l1), ch(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
    end interface radb3

    interface radb4
            subroutine radb4_sp (ido, l1, cc, ch, wa1, wa2, wa3)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 4, l1), ch(ido, l1, 4), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
            subroutine radb4_dp (ido, l1, cc, ch, wa1, wa2, wa3)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 4, l1), ch(ido, l1, 4), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
            subroutine radb4_qp (ido, l1, cc, ch, wa1, wa2, wa3)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 4, l1), ch(ido, l1, 4), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
    end interface radb4

    interface radb5
            subroutine radb5_sp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, 5, l1), ch(ido, l1, 5), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
            subroutine radb5_dp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, 5, l1), ch(ido, l1, 5), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
            subroutine radb5_qp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, 5, l1), ch(ido, l1, 5), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
    end interface radb5

    interface radbg
            subroutine radbg_sp (ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), c2(idl1, ip), &
                    ch2(idl1, ip), wa(1)
            end subroutine
            subroutine radbg_dp (ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), c2(idl1, ip), &
                    ch2(idl1, ip), wa(1)
            end subroutine
            subroutine radbg_qp (ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), c2(idl1, ip), &
                    ch2(idl1, ip), wa(1)
            end subroutine
    end interface radbg

    interface radf2
            subroutine radf2_sp (ido, l1, cc, ch, wa1)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension ch(ido, 2, l1), cc(ido, l1, 2), &
                    wa1(1)
            end subroutine
            subroutine radf2_dp (ido, l1, cc, ch, wa1)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension ch(ido, 2, l1), cc(ido, l1, 2), &
                    wa1(1)
            end subroutine
            subroutine radf2_qp (ido, l1, cc, ch, wa1)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension ch(ido, 2, l1), cc(ido, l1, 2), &
                    wa1(1)
            end subroutine
    end interface radf2

    interface radf3
            subroutine radf3_sp (ido, l1, cc, ch, wa1, wa2)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension ch(ido, 3, l1), cc(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
            subroutine radf3_dp (ido, l1, cc, ch, wa1, wa2)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension ch(ido, 3, l1), cc(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
            subroutine radf3_qp (ido, l1, cc, ch, wa1, wa2)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension ch(ido, 3, l1), cc(ido, l1, 3), &
                    wa1(1), wa2(1)
            end subroutine
    end interface radf3

    interface radf4
            subroutine radf4_sp (ido, l1, cc, ch, wa1, wa2, wa3)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, l1, 4), ch(ido, 4, l1), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
            subroutine radf4_dp (ido, l1, cc, ch, wa1, wa2, wa3)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, l1, 4), ch(ido, 4, l1), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
            subroutine radf4_qp (ido, l1, cc, ch, wa1, wa2, wa3)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, l1, 4), ch(ido, 4, l1), &
                    wa1(1), wa2(1), wa3(1)
            end subroutine
    end interface radf4

    interface radf5
            subroutine radf5_sp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension cc(ido, l1, 5), ch(ido, 5, l1), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
            subroutine radf5_dp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension cc(ido, l1, 5), ch(ido, 5, l1), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
            subroutine radf5_qp (ido, l1, cc, ch, wa1, wa2, wa3, wa4)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension cc(ido, l1, 5), ch(ido, 5, l1), &
                    wa1(1), wa2(1), wa3(1), wa4(1)
            end subroutine
    end interface radf5

    interface radfg
            subroutine radfg_sp (ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), c2(idl1, ip), &
                    ch2(idl1, ip), wa(*)
            end subroutine
            subroutine radfg_dp (ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), c2(idl1, ip), &
                    ch2(idl1, ip), wa(*)
            end subroutine
            subroutine radfg_qp (ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension ch(ido, l1, ip), cc(ido, ip, l1), &
                    c1(ido, l1, ip), c2(idl1, ip), &
                    ch2(idl1, ip), wa(*)
            end subroutine
    end interface radfg

    interface rfftb1
            subroutine rfftb1_sp (n, c, ch, wa, ifac)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
            subroutine rfftb1_dp (n, c, ch, wa, ifac)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
            subroutine rfftb1_qp (n, c, ch, wa, ifac)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
    end interface rfftb1

    interface rfftf1
            subroutine rfftf1_sp (n, c, ch, wa, ifac)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
            subroutine rfftf1_dp (n, c, ch, wa, ifac)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
            subroutine rfftf1_qp (n, c, ch, wa, ifac)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension ch(*), c(*), wa(*), ifac(*)
            end subroutine
    end interface rfftf1

    interface rffti1
            subroutine rffti1_sp (n, wa, ifac)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension wa(*), ifac(*)
            end subroutine
            subroutine rffti1_dp (n, wa, ifac)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension wa(*), ifac(*)
            end subroutine
            subroutine rffti1_qp (n, wa, ifac)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension wa(*), ifac(*)
            end subroutine
    end interface rffti1

    interface sint1
            subroutine sint1_sp (n, war, was, xh, x, ifac)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension war(*), was(*), x(*), xh(*), ifac(*)
            end subroutine
            subroutine sint1_dp (n, war, was, xh, x, ifac)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension war(*), was(*), x(*), xh(*), ifac(*)
            end subroutine
            subroutine sint1_qp (n, war, was, xh, x, ifac)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension war(*), was(*), x(*), xh(*), ifac(*)
            end subroutine
    end interface sint1

    interface zfftb
        !! version: experimental
        !!
        !! unnormalized inverse of zfftf
            subroutine zfftb_sp (n, c, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension c(*), wsave(*)
                complex(sp) c
            end subroutine
            subroutine zfftb_dp (n, c, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension c(*), wsave(*)
                complex(dp) c
            end subroutine
            subroutine zfftb_qp (n, c, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension c(*), wsave(*)
                complex(qp) c
            end subroutine
    end interface zfftb

    interface zfftf
        !! version: experimental
        !!
        !! forward transform of a complex periodic sequence
            subroutine zfftf_sp (n, c, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension c(*), wsave(*)
                complex(sp) c
            end subroutine
            subroutine zfftf_dp (n, c, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension c(*), wsave(*)
                complex(dp) c
            end subroutine
            subroutine zfftf_qp (n, c, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension c(*), wsave(*)
                complex(qp) c
            end subroutine
    end interface zfftf

    interface zffti
        !! version: experimental
        !!
        !! initialize  zfftf and zfftb
            subroutine zffti_sp (n, wsave)
                import sp
                implicit real(sp) (a - h, o - z)
                dimension wsave(*)
            end subroutine
            subroutine zffti_dp (n, wsave)
                import dp
                implicit real(dp) (a - h, o - z)
                dimension wsave(*)
            end subroutine
            subroutine zffti_qp (n, wsave)
                import qp
                implicit real(qp) (a - h, o - z)
                dimension wsave(*)
            end subroutine
    end interface zffti
contains

end module
