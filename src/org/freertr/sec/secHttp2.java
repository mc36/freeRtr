package org.freertr.sec;

import java.util.ArrayList;
import java.util.List;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.enc.encUrl;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * http2 (rfc7540) protocol
 *
 * @author matecsaba
 */
public class secHttp2 {

    /**
     * lower layer session to use for encrypted communication
     */
    public final pipeSide lower;

    /**
     * user side of cleartext pipeline
     */
    public final pipeSide userC;

    /**
     * magic command
     */
    public final static String magicCmd = "PRI * HTTP/2.0";

    /**
     * magic continue
     */
    public final static String magicCnt = "SM";

    /**
     * acknowledge
     */
    public final static int flgAck = 0x1;

    /**
     * end stream
     */
    public final static int flgStrEnd = 0x1;

    /**
     * end headers
     */
    public final static int flgHdrEnd = 0x4;

    /**
     * padded
     */
    public final static int flgPadded = 0x8;

    /**
     * priority
     */
    public final static int flgPrio = 0x20;

    /**
     * data
     */
    public final static int typData = 0;

    /**
     * headers
     */
    public final static int typHdrs = 1;

    /**
     * priority
     */
    public final static int typPrio = 2;

    /**
     * rst stream
     */
    public final static int typRst = 3;

    /**
     * settings
     */
    public final static int typSett = 4;

    /**
     * push promise
     */
    public final static int typPush = 5;

    /**
     * ping
     */
    public final static int typPing = 6;

    /**
     * goaway
     */
    public final static int typGoaway = 7;

    /**
     * window update
     */
    public final static int typWin = 8;

    /**
     * continuation
     */
    public final static int typCont = 9;

    /**
     * mode of operation, true=client, false=server
     */
    protected boolean client;

    /**
     * my side of cleartext pipeline
     */
    protected final pipeSide userS;

    /**
     * cleartext pipeline
     */
    protected final pipeLine userP;

    /**
     * hpack huffman table
     */
    protected static List<secHttp2huf> hpackHuf;

    /**
     * frame type
     */
    protected int frTyp;

    /**
     * frame flags
     */
    protected int frFlg;

    /**
     * stream identifier
     */
    protected int frStr;

    /**
     * current stream identifier
     */
    protected int curStr = 1;

    /**
     * data offset
     */
    protected int datOfs;

    /**
     * data size
     */
    protected int datSiz;

    /**
     * close requested
     */
    protected boolean clsReq;

    /**
     * convert type to string
     *
     * @param i opcode
     * @return string
     */
    public final static String type2string(int i) {
        switch (i) {
            case typData:
                return "data";
            case typHdrs:
                return "headers";
            case typPrio:
                return "priority";
            case typRst:
                return "reset";
            case typSett:
                return "settings";
            case typPush:
                return "push";
            case typPing:
                return "ping";
            case typGoaway:
                return "goaway";
            case typWin:
                return "window";
            case typCont:
                return "continuation";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * start http2 on a session
     *
     * @param session pipeside to use as lower layer
     * @param pipe pipeline to use on user side
     */
    public secHttp2(pipeSide session, pipeLine pipe) {
        lower = session;
        lower.setTime(120 * 1000);
        userP = pipe;
        userS = pipe.getSide();
        userC = pipe.getSide();
        userC.setTime(120 * 1000);
        userS.setTime(120 * 1000);
        userC.lineRx = pipeSide.modTyp.modeCRtryLF;
        userC.lineTx = pipeSide.modTyp.modeCRLF;
        userS.lineRx = pipeSide.modTyp.modeCRtryLF;
        userS.lineTx = pipeSide.modTyp.modeCRLF;
        hpackInit();
    }

    /**
     * initialize hpack
     */
    public final static void hpackInit() {
        if (hpackHuf != null) {
            return;
        }
        hpackHuf = new ArrayList<secHttp2huf>();
        hpackHuf.add(new secHttp2huf(48, 0x0, 5));
        hpackHuf.add(new secHttp2huf(49, 0x1, 5));
        hpackHuf.add(new secHttp2huf(50, 0x2, 5));
        hpackHuf.add(new secHttp2huf(97, 0x3, 5));
        hpackHuf.add(new secHttp2huf(99, 0x4, 5));
        hpackHuf.add(new secHttp2huf(101, 0x5, 5));
        hpackHuf.add(new secHttp2huf(105, 0x6, 5));
        hpackHuf.add(new secHttp2huf(111, 0x7, 5));
        hpackHuf.add(new secHttp2huf(115, 0x8, 5));
        hpackHuf.add(new secHttp2huf(116, 0x9, 5));
        hpackHuf.add(new secHttp2huf(32, 0x14, 6));
        hpackHuf.add(new secHttp2huf(37, 0x15, 6));
        hpackHuf.add(new secHttp2huf(45, 0x16, 6));
        hpackHuf.add(new secHttp2huf(46, 0x17, 6));
        hpackHuf.add(new secHttp2huf(47, 0x18, 6));
        hpackHuf.add(new secHttp2huf(51, 0x19, 6));
        hpackHuf.add(new secHttp2huf(52, 0x1a, 6));
        hpackHuf.add(new secHttp2huf(53, 0x1b, 6));
        hpackHuf.add(new secHttp2huf(54, 0x1c, 6));
        hpackHuf.add(new secHttp2huf(55, 0x1d, 6));
        hpackHuf.add(new secHttp2huf(56, 0x1e, 6));
        hpackHuf.add(new secHttp2huf(57, 0x1f, 6));
        hpackHuf.add(new secHttp2huf(61, 0x20, 6));
        hpackHuf.add(new secHttp2huf(65, 0x21, 6));
        hpackHuf.add(new secHttp2huf(95, 0x22, 6));
        hpackHuf.add(new secHttp2huf(98, 0x23, 6));
        hpackHuf.add(new secHttp2huf(100, 0x24, 6));
        hpackHuf.add(new secHttp2huf(102, 0x25, 6));
        hpackHuf.add(new secHttp2huf(103, 0x26, 6));
        hpackHuf.add(new secHttp2huf(104, 0x27, 6));
        hpackHuf.add(new secHttp2huf(108, 0x28, 6));
        hpackHuf.add(new secHttp2huf(109, 0x29, 6));
        hpackHuf.add(new secHttp2huf(110, 0x2a, 6));
        hpackHuf.add(new secHttp2huf(112, 0x2b, 6));
        hpackHuf.add(new secHttp2huf(114, 0x2c, 6));
        hpackHuf.add(new secHttp2huf(117, 0x2d, 6));
        hpackHuf.add(new secHttp2huf(58, 0x5c, 7));
        hpackHuf.add(new secHttp2huf(66, 0x5d, 7));
        hpackHuf.add(new secHttp2huf(67, 0x5e, 7));
        hpackHuf.add(new secHttp2huf(68, 0x5f, 7));
        hpackHuf.add(new secHttp2huf(69, 0x60, 7));
        hpackHuf.add(new secHttp2huf(70, 0x61, 7));
        hpackHuf.add(new secHttp2huf(71, 0x62, 7));
        hpackHuf.add(new secHttp2huf(72, 0x63, 7));
        hpackHuf.add(new secHttp2huf(73, 0x64, 7));
        hpackHuf.add(new secHttp2huf(74, 0x65, 7));
        hpackHuf.add(new secHttp2huf(75, 0x66, 7));
        hpackHuf.add(new secHttp2huf(76, 0x67, 7));
        hpackHuf.add(new secHttp2huf(77, 0x68, 7));
        hpackHuf.add(new secHttp2huf(78, 0x69, 7));
        hpackHuf.add(new secHttp2huf(79, 0x6a, 7));
        hpackHuf.add(new secHttp2huf(80, 0x6b, 7));
        hpackHuf.add(new secHttp2huf(81, 0x6c, 7));
        hpackHuf.add(new secHttp2huf(82, 0x6d, 7));
        hpackHuf.add(new secHttp2huf(83, 0x6e, 7));
        hpackHuf.add(new secHttp2huf(84, 0x6f, 7));
        hpackHuf.add(new secHttp2huf(85, 0x70, 7));
        hpackHuf.add(new secHttp2huf(86, 0x71, 7));
        hpackHuf.add(new secHttp2huf(87, 0x72, 7));
        hpackHuf.add(new secHttp2huf(89, 0x73, 7));
        hpackHuf.add(new secHttp2huf(106, 0x74, 7));
        hpackHuf.add(new secHttp2huf(107, 0x75, 7));
        hpackHuf.add(new secHttp2huf(113, 0x76, 7));
        hpackHuf.add(new secHttp2huf(118, 0x77, 7));
        hpackHuf.add(new secHttp2huf(119, 0x78, 7));
        hpackHuf.add(new secHttp2huf(120, 0x79, 7));
        hpackHuf.add(new secHttp2huf(121, 0x7a, 7));
        hpackHuf.add(new secHttp2huf(122, 0x7b, 7));
        hpackHuf.add(new secHttp2huf(38, 0xf8, 8));
        hpackHuf.add(new secHttp2huf(42, 0xf9, 8));
        hpackHuf.add(new secHttp2huf(44, 0xfa, 8));
        hpackHuf.add(new secHttp2huf(59, 0xfb, 8));
        hpackHuf.add(new secHttp2huf(88, 0xfc, 8));
        hpackHuf.add(new secHttp2huf(90, 0xfd, 8));
        hpackHuf.add(new secHttp2huf(33, 0x3f8, 10));
        hpackHuf.add(new secHttp2huf(34, 0x3f9, 10));
        hpackHuf.add(new secHttp2huf(40, 0x3fa, 10));
        hpackHuf.add(new secHttp2huf(41, 0x3fb, 10));
        hpackHuf.add(new secHttp2huf(63, 0x3fc, 10));
        hpackHuf.add(new secHttp2huf(39, 0x7fa, 11));
        hpackHuf.add(new secHttp2huf(43, 0x7fb, 11));
        hpackHuf.add(new secHttp2huf(124, 0x7fc, 11));
        hpackHuf.add(new secHttp2huf(35, 0xffa, 12));
        hpackHuf.add(new secHttp2huf(62, 0xffb, 12));
        hpackHuf.add(new secHttp2huf(0, 0x1ff8, 13));
        hpackHuf.add(new secHttp2huf(36, 0x1ff9, 13));
        hpackHuf.add(new secHttp2huf(64, 0x1ffa, 13));
        hpackHuf.add(new secHttp2huf(91, 0x1ffb, 13));
        hpackHuf.add(new secHttp2huf(93, 0x1ffc, 13));
        hpackHuf.add(new secHttp2huf(126, 0x1ffd, 13));
        hpackHuf.add(new secHttp2huf(94, 0x3ffc, 14));
        hpackHuf.add(new secHttp2huf(125, 0x3ffd, 14));
        hpackHuf.add(new secHttp2huf(60, 0x7ffc, 15));
        hpackHuf.add(new secHttp2huf(96, 0x7ffd, 15));
        hpackHuf.add(new secHttp2huf(123, 0x7ffe, 15));
        hpackHuf.add(new secHttp2huf(92, 0x7fff0, 19));
        hpackHuf.add(new secHttp2huf(195, 0x7fff1, 19));
        hpackHuf.add(new secHttp2huf(208, 0x7fff2, 19));
        hpackHuf.add(new secHttp2huf(128, 0xfffe6, 20));
        hpackHuf.add(new secHttp2huf(130, 0xfffe7, 20));
        hpackHuf.add(new secHttp2huf(131, 0xfffe8, 20));
        hpackHuf.add(new secHttp2huf(162, 0xfffe9, 20));
        hpackHuf.add(new secHttp2huf(184, 0xfffea, 20));
        hpackHuf.add(new secHttp2huf(194, 0xfffeb, 20));
        hpackHuf.add(new secHttp2huf(224, 0xfffec, 20));
        hpackHuf.add(new secHttp2huf(226, 0xfffed, 20));
        hpackHuf.add(new secHttp2huf(153, 0x1fffdc, 21));
        hpackHuf.add(new secHttp2huf(161, 0x1fffdd, 21));
        hpackHuf.add(new secHttp2huf(167, 0x1fffde, 21));
        hpackHuf.add(new secHttp2huf(172, 0x1fffdf, 21));
        hpackHuf.add(new secHttp2huf(176, 0x1fffe0, 21));
        hpackHuf.add(new secHttp2huf(177, 0x1fffe1, 21));
        hpackHuf.add(new secHttp2huf(179, 0x1fffe2, 21));
        hpackHuf.add(new secHttp2huf(209, 0x1fffe3, 21));
        hpackHuf.add(new secHttp2huf(216, 0x1fffe4, 21));
        hpackHuf.add(new secHttp2huf(217, 0x1fffe5, 21));
        hpackHuf.add(new secHttp2huf(227, 0x1fffe6, 21));
        hpackHuf.add(new secHttp2huf(229, 0x1fffe7, 21));
        hpackHuf.add(new secHttp2huf(230, 0x1fffe8, 21));
        hpackHuf.add(new secHttp2huf(129, 0x3fffd2, 22));
        hpackHuf.add(new secHttp2huf(132, 0x3fffd3, 22));
        hpackHuf.add(new secHttp2huf(133, 0x3fffd4, 22));
        hpackHuf.add(new secHttp2huf(134, 0x3fffd5, 22));
        hpackHuf.add(new secHttp2huf(136, 0x3fffd6, 22));
        hpackHuf.add(new secHttp2huf(146, 0x3fffd7, 22));
        hpackHuf.add(new secHttp2huf(154, 0x3fffd8, 22));
        hpackHuf.add(new secHttp2huf(156, 0x3fffd9, 22));
        hpackHuf.add(new secHttp2huf(160, 0x3fffda, 22));
        hpackHuf.add(new secHttp2huf(163, 0x3fffdb, 22));
        hpackHuf.add(new secHttp2huf(164, 0x3fffdc, 22));
        hpackHuf.add(new secHttp2huf(169, 0x3fffdd, 22));
        hpackHuf.add(new secHttp2huf(170, 0x3fffde, 22));
        hpackHuf.add(new secHttp2huf(173, 0x3fffdf, 22));
        hpackHuf.add(new secHttp2huf(178, 0x3fffe0, 22));
        hpackHuf.add(new secHttp2huf(181, 0x3fffe1, 22));
        hpackHuf.add(new secHttp2huf(185, 0x3fffe2, 22));
        hpackHuf.add(new secHttp2huf(186, 0x3fffe3, 22));
        hpackHuf.add(new secHttp2huf(187, 0x3fffe4, 22));
        hpackHuf.add(new secHttp2huf(189, 0x3fffe5, 22));
        hpackHuf.add(new secHttp2huf(190, 0x3fffe6, 22));
        hpackHuf.add(new secHttp2huf(196, 0x3fffe7, 22));
        hpackHuf.add(new secHttp2huf(198, 0x3fffe8, 22));
        hpackHuf.add(new secHttp2huf(228, 0x3fffe9, 22));
        hpackHuf.add(new secHttp2huf(232, 0x3fffea, 22));
        hpackHuf.add(new secHttp2huf(233, 0x3fffeb, 22));
        hpackHuf.add(new secHttp2huf(1, 0x7fffd8, 23));
        hpackHuf.add(new secHttp2huf(135, 0x7fffd9, 23));
        hpackHuf.add(new secHttp2huf(137, 0x7fffda, 23));
        hpackHuf.add(new secHttp2huf(138, 0x7fffdb, 23));
        hpackHuf.add(new secHttp2huf(139, 0x7fffdc, 23));
        hpackHuf.add(new secHttp2huf(140, 0x7fffdd, 23));
        hpackHuf.add(new secHttp2huf(141, 0x7fffde, 23));
        hpackHuf.add(new secHttp2huf(143, 0x7fffdf, 23));
        hpackHuf.add(new secHttp2huf(147, 0x7fffe0, 23));
        hpackHuf.add(new secHttp2huf(149, 0x7fffe1, 23));
        hpackHuf.add(new secHttp2huf(150, 0x7fffe2, 23));
        hpackHuf.add(new secHttp2huf(151, 0x7fffe3, 23));
        hpackHuf.add(new secHttp2huf(152, 0x7fffe4, 23));
        hpackHuf.add(new secHttp2huf(155, 0x7fffe5, 23));
        hpackHuf.add(new secHttp2huf(157, 0x7fffe6, 23));
        hpackHuf.add(new secHttp2huf(158, 0x7fffe7, 23));
        hpackHuf.add(new secHttp2huf(165, 0x7fffe8, 23));
        hpackHuf.add(new secHttp2huf(166, 0x7fffe9, 23));
        hpackHuf.add(new secHttp2huf(168, 0x7fffea, 23));
        hpackHuf.add(new secHttp2huf(174, 0x7fffeb, 23));
        hpackHuf.add(new secHttp2huf(175, 0x7fffec, 23));
        hpackHuf.add(new secHttp2huf(180, 0x7fffed, 23));
        hpackHuf.add(new secHttp2huf(182, 0x7fffee, 23));
        hpackHuf.add(new secHttp2huf(183, 0x7fffef, 23));
        hpackHuf.add(new secHttp2huf(188, 0x7ffff0, 23));
        hpackHuf.add(new secHttp2huf(191, 0x7ffff1, 23));
        hpackHuf.add(new secHttp2huf(197, 0x7ffff2, 23));
        hpackHuf.add(new secHttp2huf(231, 0x7ffff3, 23));
        hpackHuf.add(new secHttp2huf(239, 0x7ffff4, 23));
        hpackHuf.add(new secHttp2huf(9, 0xffffea, 24));
        hpackHuf.add(new secHttp2huf(142, 0xffffeb, 24));
        hpackHuf.add(new secHttp2huf(144, 0xffffec, 24));
        hpackHuf.add(new secHttp2huf(145, 0xffffed, 24));
        hpackHuf.add(new secHttp2huf(148, 0xffffee, 24));
        hpackHuf.add(new secHttp2huf(159, 0xffffef, 24));
        hpackHuf.add(new secHttp2huf(171, 0xfffff0, 24));
        hpackHuf.add(new secHttp2huf(206, 0xfffff1, 24));
        hpackHuf.add(new secHttp2huf(215, 0xfffff2, 24));
        hpackHuf.add(new secHttp2huf(225, 0xfffff3, 24));
        hpackHuf.add(new secHttp2huf(236, 0xfffff4, 24));
        hpackHuf.add(new secHttp2huf(237, 0xfffff5, 24));
        hpackHuf.add(new secHttp2huf(199, 0x1ffffec, 25));
        hpackHuf.add(new secHttp2huf(207, 0x1ffffed, 25));
        hpackHuf.add(new secHttp2huf(234, 0x1ffffee, 25));
        hpackHuf.add(new secHttp2huf(235, 0x1ffffef, 25));
        hpackHuf.add(new secHttp2huf(192, 0x3ffffe0, 26));
        hpackHuf.add(new secHttp2huf(193, 0x3ffffe1, 26));
        hpackHuf.add(new secHttp2huf(200, 0x3ffffe2, 26));
        hpackHuf.add(new secHttp2huf(201, 0x3ffffe3, 26));
        hpackHuf.add(new secHttp2huf(202, 0x3ffffe4, 26));
        hpackHuf.add(new secHttp2huf(205, 0x3ffffe5, 26));
        hpackHuf.add(new secHttp2huf(210, 0x3ffffe6, 26));
        hpackHuf.add(new secHttp2huf(213, 0x3ffffe7, 26));
        hpackHuf.add(new secHttp2huf(218, 0x3ffffe8, 26));
        hpackHuf.add(new secHttp2huf(219, 0x3ffffe9, 26));
        hpackHuf.add(new secHttp2huf(238, 0x3ffffea, 26));
        hpackHuf.add(new secHttp2huf(240, 0x3ffffeb, 26));
        hpackHuf.add(new secHttp2huf(242, 0x3ffffec, 26));
        hpackHuf.add(new secHttp2huf(243, 0x3ffffed, 26));
        hpackHuf.add(new secHttp2huf(255, 0x3ffffee, 26));
        hpackHuf.add(new secHttp2huf(203, 0x7ffffde, 27));
        hpackHuf.add(new secHttp2huf(204, 0x7ffffdf, 27));
        hpackHuf.add(new secHttp2huf(211, 0x7ffffe0, 27));
        hpackHuf.add(new secHttp2huf(212, 0x7ffffe1, 27));
        hpackHuf.add(new secHttp2huf(214, 0x7ffffe2, 27));
        hpackHuf.add(new secHttp2huf(221, 0x7ffffe3, 27));
        hpackHuf.add(new secHttp2huf(222, 0x7ffffe4, 27));
        hpackHuf.add(new secHttp2huf(223, 0x7ffffe5, 27));
        hpackHuf.add(new secHttp2huf(241, 0x7ffffe6, 27));
        hpackHuf.add(new secHttp2huf(244, 0x7ffffe7, 27));
        hpackHuf.add(new secHttp2huf(245, 0x7ffffe8, 27));
        hpackHuf.add(new secHttp2huf(246, 0x7ffffe9, 27));
        hpackHuf.add(new secHttp2huf(247, 0x7ffffea, 27));
        hpackHuf.add(new secHttp2huf(248, 0x7ffffeb, 27));
        hpackHuf.add(new secHttp2huf(250, 0x7ffffec, 27));
        hpackHuf.add(new secHttp2huf(251, 0x7ffffed, 27));
        hpackHuf.add(new secHttp2huf(252, 0x7ffffee, 27));
        hpackHuf.add(new secHttp2huf(253, 0x7ffffef, 27));
        hpackHuf.add(new secHttp2huf(254, 0x7fffff0, 27));
        hpackHuf.add(new secHttp2huf(2, 0xfffffe2, 28));
        hpackHuf.add(new secHttp2huf(3, 0xfffffe3, 28));
        hpackHuf.add(new secHttp2huf(4, 0xfffffe4, 28));
        hpackHuf.add(new secHttp2huf(5, 0xfffffe5, 28));
        hpackHuf.add(new secHttp2huf(6, 0xfffffe6, 28));
        hpackHuf.add(new secHttp2huf(7, 0xfffffe7, 28));
        hpackHuf.add(new secHttp2huf(8, 0xfffffe8, 28));
        hpackHuf.add(new secHttp2huf(11, 0xfffffe9, 28));
        hpackHuf.add(new secHttp2huf(12, 0xfffffea, 28));
        hpackHuf.add(new secHttp2huf(14, 0xfffffeb, 28));
        hpackHuf.add(new secHttp2huf(15, 0xfffffec, 28));
        hpackHuf.add(new secHttp2huf(16, 0xfffffed, 28));
        hpackHuf.add(new secHttp2huf(17, 0xfffffee, 28));
        hpackHuf.add(new secHttp2huf(18, 0xfffffef, 28));
        hpackHuf.add(new secHttp2huf(19, 0xffffff0, 28));
        hpackHuf.add(new secHttp2huf(20, 0xffffff1, 28));
        hpackHuf.add(new secHttp2huf(21, 0xffffff2, 28));
        hpackHuf.add(new secHttp2huf(23, 0xffffff3, 28));
        hpackHuf.add(new secHttp2huf(24, 0xffffff4, 28));
        hpackHuf.add(new secHttp2huf(25, 0xffffff5, 28));
        hpackHuf.add(new secHttp2huf(26, 0xffffff6, 28));
        hpackHuf.add(new secHttp2huf(27, 0xffffff7, 28));
        hpackHuf.add(new secHttp2huf(28, 0xffffff8, 28));
        hpackHuf.add(new secHttp2huf(29, 0xffffff9, 28));
        hpackHuf.add(new secHttp2huf(30, 0xffffffa, 28));
        hpackHuf.add(new secHttp2huf(31, 0xffffffb, 28));
        hpackHuf.add(new secHttp2huf(127, 0xffffffc, 28));
        hpackHuf.add(new secHttp2huf(220, 0xffffffd, 28));
        hpackHuf.add(new secHttp2huf(249, 0xffffffe, 28));
        hpackHuf.add(new secHttp2huf(10, 0x3ffffffc, 30));
        hpackHuf.add(new secHttp2huf(13, 0x3ffffffd, 30));
        hpackHuf.add(new secHttp2huf(22, 0x3ffffffe, 30));
        hpackHuf.add(new secHttp2huf(256, 0x3fffffff, 30));
    }

    /**
     * read static table
     *
     * @param idx index
     * @return value, null if not found
     */
    public final static String hpackStable(int idx) {
        switch (idx) {
            case 1:
                return ":authority: ";
            case 2:
                return ":method: GET";
            case 3:
                return ":method: POST";
            case 4:
                return ":path: /";
            case 5:
                return ":path: /index.html";
            case 6:
                return ":scheme: http";
            case 7:
                return ":scheme: https";
            case 8:
                return ":status: 200";
            case 9:
                return ":status: 204";
            case 10:
                return ":status: 206";
            case 11:
                return ":status: 304";
            case 12:
                return ":status: 400";
            case 13:
                return ":status: 404";
            case 14:
                return ":status: 500";
            case 15:
                return "accept-charset: ";
            case 16:
                return "accept-encoding: gzip, deflate";
            case 17:
                return "accept-language: ";
            case 18:
                return "accept-ranges: ";
            case 19:
                return "accept: ";
            case 20:
                return "access-control-allow-origin: ";
            case 21:
                return "age: ";
            case 22:
                return "allow: ";
            case 23:
                return "authorization: ";
            case 24:
                return "cache-control: ";
            case 25:
                return "content-disposition: ";
            case 26:
                return "content-encoding: ";
            case 27:
                return "content-language: ";
            case 28:
                return "content-length: ";
            case 29:
                return "content-location: ";
            case 30:
                return "content-range: ";
            case 31:
                return "content-type: ";
            case 32:
                return "cookie: ";
            case 33:
                return "date: ";
            case 34:
                return "etag: ";
            case 35:
                return "expect: ";
            case 36:
                return "expires: ";
            case 37:
                return "from: ";
            case 38:
                return "host: ";
            case 39:
                return "if-match: ";
            case 40:
                return "if-modified-since: ";
            case 41:
                return "if-none-match: ";
            case 42:
                return "if-range: ";
            case 43:
                return "if-unmodified-since: ";
            case 44:
                return "last-modified: ";
            case 45:
                return "link: ";
            case 46:
                return "location: ";
            case 47:
                return "max-forwards: ";
            case 48:
                return "proxy-authenticate: ";
            case 49:
                return "proxy-authorization: ";
            case 50:
                return "range: ";
            case 51:
                return "referer: ";
            case 52:
                return "refresh: ";
            case 53:
                return "retry-after: ";
            case 54:
                return "server: ";
            case 55:
                return "set-cookie: ";
            case 56:
                return "strict-transport-security: ";
            case 57:
                return "transfer-encoding: ";
            case 58:
                return "user-agent: ";
            case 59:
                return "vary: ";
            case 60:
                return "via: ";
            case 61:
                return "www-authenticate: ";
            default:
                return null;
        }
    }

    /**
     * decode integer
     *
     * @param pck encoded
     * @param lim 1 shl size - 1
     * @return value
     */
    public final static int hpackGetInt(packHolder pck, int lim) {
        int val = pck.getByte(0) & lim;
        pck.getSkip(1);
        if (val < lim) {
            return val;
        }
        val = 0;
        for (int pos = 0;; pos += 7) {
            int cur = pck.getByte(0);
            pck.getSkip(1);
            val |= (cur & 0x7f) << pos;
            if ((cur & 0x80) == 0) {
                break;
            }
        }
        return val;
    }

    /**
     * encode integer
     *
     * @param pck encoded
     * @param pre prefix
     * @param lim 1 shl size - 1
     * @param val value
     */
    public final static void hpackPutInt(packHolder pck, int lim, int pre, int val) {
        if (val < lim) {
            pck.putByte(0, pre | val);
            pck.putSkip(1);
            return;
        }
        pck.putByte(0, pre | lim);
        pck.putSkip(1);
        for (;;) {
            int cur = val & 0x7f;
            val = val >>> 7;
            if (val == 0) {
                pck.putByte(0, cur);
                pck.putSkip(1);
                return;
            }
            pck.putByte(0, 0x80 | cur);
            pck.putSkip(1);
        }
    }

    /**
     * decode string
     *
     * @param pck encoded
     * @return string, null if error
     */
    public final static String hpackGetStr(packHolder pck) {
        boolean huffed = (pck.getByte(0) & 0x80) != 0;
        int len = hpackGetInt(pck, 0x7f);
        if (len > pck.dataSize()) {
            return null;
        }
        if (!huffed) {
            byte[] buf = new byte[len];
            pck.getCopy(buf, 0, 0, len);
            pck.getSkip(buf.length);
            return new String(buf);
        }
        String res = "";
        int beg = 0;
        for (;;) {
            int val = 0;
            int siz = 0;
            int end = 0;
            secHttp2huf huf = null;
            for (int cod = 0; cod < 256; cod++) {
                huf = hpackHuf.get(cod);
                val <<= huf.siz - siz;
                for (; siz < huf.siz; siz++) {
                    int pos = beg + siz;
                    if ((pos >>> 3) >= len) {
                        end = 2;
                    }
                    if ((pck.getByte(pos >>> 3) & bits.bitVals[7 - (pos & 7)]) == 0) {
                        continue;
                    }
                    val |= 1 << (huf.siz - siz - 1);
                }
                if (end > 0) {
                    break;
                }
                siz = huf.siz;
                if (val == huf.val) {
                    end = 1;
                    break;
                }
            }
            if (end < 1) {
                return null;
            }
            if (end > 1) {
                break;
            }
            res += (char) huf.asc;
            beg += siz;
        }
        pck.getSkip(len);
        return res;
    }

    /**
     * encode string
     *
     * @param pck encoded
     * @param val value
     */
    public final static void hpackPutStr(packHolder pck, String val) {
        byte[] buf = val.getBytes();
        hpackPutInt(pck, 0x7f, 0, buf.length);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
    }

    /**
     * decode string
     *
     * @param pck encoded
     * @param cur index
     * @return string, null if error
     */
    public final static String hpackGetLit(packHolder pck, int cur) {
        String nam;
        if (cur == 0) {
            nam = hpackGetStr(pck) + ": ";
        } else {
            nam = hpackStable(cur);
        }
        if (nam == null) {
            return null;
        }
        int i = nam.lastIndexOf(":");
        if (i < 0) {
            return null;
        }
        nam = nam.substring(0, i);
        String val = hpackGetStr(pck);
        if (val == null) {
            return null;
        }
        return nam + ": " + val;
    }

    /**
     * encode literal
     *
     * @param pck encoded
     * @param nam name
     * @param val value
     */
    public final static void hpackPutLit(packHolder pck, int nam, String val) {
        pck.putByte(0, 0x40 | (nam & 0x2f));
        pck.putSkip(1);
        hpackPutStr(pck, val);
    }

    /**
     * encode literal
     *
     * @param pck encoded
     * @param nam name
     * @param val value
     */
    public final static void hpackPutLit(packHolder pck, String nam, String val) {
        pck.putByte(0, 0);
        pck.putSkip(1);
        hpackPutStr(pck, nam);
        hpackPutStr(pck, val);
    }

    /**
     * decode header
     *
     * @param pck encoded
     * @param req client side
     * @return decoded, null if error
     */
    public final static List<String> hpackDecode(packHolder pck, boolean req) {
        if (debugger.secHttp2traf) {
            logger.debug("dec hdr " + req + " " + pck.dump());
        }
        List<String> res = new ArrayList<String>();
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            int cur = pck.getByte(0);
            if ((cur & 0x80) == 0x80) { // indexed
                cur = hpackGetInt(pck, 0x7f);
                String val = hpackStable(cur);
                if (val == null) {
                    return null;
                }
                res.add(val);
                continue;
            }
            if ((cur & 0xc0) == 0x40) { // incremental index
                cur = hpackGetInt(pck, 0x3f);
                String val = hpackGetLit(pck, cur & 0x3f);
                if (val == null) {
                    return null;
                }
                res.add(val);
                continue;
            }
            if ((cur & 0xf0) == 0x00) { // no index
                cur = hpackGetInt(pck, 0x0f);
                String val = hpackGetLit(pck, cur & 0x0f);
                if (val == null) {
                    return null;
                }
                res.add(val);
                continue;
            }
            if ((cur & 0xf0) == 0x10) { // never index
                cur = hpackGetInt(pck, 0x0f);
                String val = hpackGetLit(pck, cur & 0x0f);
                if (val == null) {
                    return null;
                }
                res.add(val);
                continue;
            }
            if ((cur & 0xe0) == 0x20) { // table update
                hpackGetInt(pck, 0x1f);
                continue;
            }
            return null;
        }
        String meth = "get";
        String path = "/";
        String schm = "http";
        String stat = "200";
        String auth = "";
        for (int i = res.size() - 1; i >= 0; i--) {
            String a = res.get(i);
            if (!a.startsWith(":")) {
                continue;
            }
            res.remove(i);
            if (a.startsWith(":method:")) {
                meth = a.substring(8, a.length());
                continue;
            }
            if (a.startsWith(":path:")) {
                path = a.substring(6, a.length());
                continue;
            }
            if (a.startsWith(":scheme:")) {
                schm = a.substring(8, a.length());
                continue;
            }
            if (a.startsWith(":authority:")) {
                auth = a.substring(11, a.length());
                continue;
            }
            if (a.startsWith(":status:")) {
                stat = a.substring(8, a.length());
                continue;
            }
        }
        if (req) {
            path = path.trim();
            if (!path.startsWith("/")) {
                path = "/" + path;
            }
            res.add(0, meth.trim() + " " + schm.trim() + "://" + auth.trim() + path + " HTTP/1.1");
            res.add(1, "connection: keep-alive");
        } else {
            res.add(0, stat.trim() + " status");
        }
        res.add("");
        return res;
    }

    /**
     * encode header
     *
     * @param hdr headers
     * @param req request
     * @return encoded, null if error
     */
    public final static packHolder hpackEncode(List<String> hdr, boolean req) {
        if (hdr.size() < 1) {
            return null;
        }
        packHolder pck = new packHolder(true, true);
        if (req) {
            String a = hdr.get(0);
            int i = a.indexOf(" ");
            if (i < 0) {
                return null;
            }
            hpackPutLit(pck, ":method", a.substring(0, i).trim());
            a = a.substring(i, a.length()).trim();
            i = a.lastIndexOf(" ");
            if (i < 0) {
                return null;
            }
            a = a.substring(0, i).trim();
            encUrl url = new encUrl();
            if (url.fromString(a)) {
                return null;
            }
            if (url.proto.length() > 0) {
                hpackPutLit(pck, ":scheme", url.proto);
            }
            if (url.server.length() > 0) {
                hpackPutLit(pck, ":authority", url.server);
            }
            hpackPutLit(pck, ":path", "/" + url.toPathName());
        } else {
            String a = hdr.get(0);
            int i = a.indexOf(" ");
            if (i < 0) {
                return null;
            }
            a = a.substring(i, a.length()).trim();
            i = a.indexOf(" ");
            if (i < 0) {
                return null;
            }
            hpackPutLit(pck, ":status", a.substring(0, i));
        }
        pck.merge2end();
        for (int o = 1; o < hdr.size(); o++) {
            String a = hdr.get(o);
            int i = a.indexOf(":");
            if (i < 0) {
                return null;
            }
            String s = a.substring(i + 1, a.length()).trim();
            a = a.substring(0, i).trim().toLowerCase();
            if (a.equals("upgrade")) {
                continue;
            }
            if (a.equals("connection")) {
                continue;
            }
            if (a.equals("keep-alive")) {
                continue;
            }
            if (a.equals("host")) {
                a = ":authority";
            }
            hpackPutLit(pck, a, s);
            pck.merge2end();
        }
        if (debugger.secHttp2traf) {
            logger.debug("enc hdr " + req + " " + pck.dump());
        }
        return pck;
    }

    /**
     * get user side pipeline
     *
     * @return cleartext pipeline
     */
    public pipeSide getPipe() {
        return userC;
    }

    /**
     * start client connection
     *
     * @return false on success, true on error;
     */
    public boolean startClient() {
        client = true;
        lower.linePut(magicCmd);
        lower.linePut("");
        lower.linePut(magicCnt);
        lower.linePut("");
        return workerStart();
    }

    /**
     * start server connection
     *
     * @param sawPri saw pri line
     * @return false on success, true on error;
     */
    public boolean startServer(boolean sawPri) {
        client = false;
        if (!sawPri) {
            if (!lower.lineGet(1).equals(magicCmd)) {
                return true;
            }
        }
        if (lower.lineGet(1).length() > 0) {
            return true;
        }
        if (!lower.lineGet(1).equals(magicCnt)) {
            return true;
        }
        if (lower.lineGet(1).length() > 0) {
            return true;
        }
        return workerStart();
    }

    private byte[] recvFrame() {
        byte[] buf = new byte[9];
        if (lower.moreGet(buf, 0, buf.length) != buf.length) {
            return null;
        }
        int len = bits.msbGetD(buf, 0) >>> 8; // length
        frTyp = buf[3]; // type
        frFlg = buf[4]; // flags
        frStr = bits.msbGetD(buf, 5); // stream id
        buf = new byte[len];
        if (debugger.secHttp2traf) {
            logger.debug("rx " + type2string(frTyp) + " flg=" + frFlg + " str=" + frStr + " len=" + buf.length);
        }
        if (len > 0x8000) {
            return null;
        }
        if (lower.moreGet(buf, 0, buf.length) != buf.length) {
            return null;
        }
        return buf;
    }

    private synchronized boolean sendFrame(int typ, int flg, int str, byte[] dat) {
        if (debugger.secHttp2traf) {
            logger.debug("tx " + type2string(typ) + " flg=" + flg + " str=" + str + " len=" + dat.length);
        }
        byte[] hdr = new byte[9];
        bits.msbPutD(hdr, 0, dat.length << 8); // length
        hdr[3] = (byte) typ; // type
        hdr[4] = (byte) flg; // flags
        bits.msbPutD(hdr, 5, str); // stream id
        if (lower.morePut(hdr, 0, hdr.length) != hdr.length) {
            return true;
        }
        return lower.morePut(dat, 0, dat.length) != dat.length;
    }

    private boolean sendSettings(int strm) {
        byte[] buf = new byte[18];
        bits.msbPutW(buf, 0, 1); // header table size
        bits.msbPutD(buf, 2, 0); // value
        bits.msbPutW(buf, 6, 2); // enable push
        bits.msbPutD(buf, 8, 0); // value
        bits.msbPutW(buf, 12, 3); // max streams
        bits.msbPutD(buf, 14, strm); // value
        return sendFrame(typSett, 0, 0, buf);
    }

    private boolean sendRst(int strm) {
        byte[] buf = new byte[4];
        return sendFrame(typRst, 0, strm, buf);
    }

    private boolean sendWindow(int strm, int win) {
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, win); // value
        return sendFrame(typWin, 0, strm, buf);
    }

    private boolean workerStart() {
        if (debugger.secHttp2traf) {
            logger.debug("starting");
        }
        sendSettings(1);
        if (recvFrame() == null) {
            return true;
        }
        if (frTyp != typSett) {
            return true;
        }
        if (sendFrame(typSett, 1, 0, new byte[0])) { // ack
            return true;
        }
        userS.setReady();
        new secHttp2rx(this);
        new secHttp2tx(this);
        return false;
    }

    /**
     * stop threads
     */
    protected synchronized void workerStop() {
        byte[] buf = new byte[8];
        bits.msbPutD(buf, 0, curStr); // stream id
        bits.msbPutD(buf, 4, 0); // error code
        sendFrame(typGoaway, 0, 0, buf); // ack
        userP.setClose();
        lower.setClose();
    }

    /**
     * sender worker
     */
    protected void workerTx() {
        if (debugger.secWebsockTraf) {
            logger.debug("tx started");
        }
        for (;;) {
            List<String> hdr = new ArrayList<String>();
            long len = -1;
            clsReq = false;
            for (;;) {
                String a = userS.lineGet(1);
                if (a.length() < 1) {
                    break;
                }
                hdr.add(a);
                a = a.toLowerCase();
                if (a.startsWith("connection:")) {
                    clsReq = a.indexOf("close") >= 0;
                    continue;
                }
                if (a.startsWith("content-length:")) {
                    len = bits.str2long(a.substring(15, a.length()));
                    continue;
                }
            }
            if (hdr.size() < 1) {
                return;
            }
            packHolder hd = hpackEncode(hdr, client);
            if (hd == null) {
                return;
            }
            if (client) {
                curStr += 2;
            }
            int typ = typHdrs;
            for (;;) {
                int siz = hd.dataSize();
                if (siz < 1) {
                    break;
                }
                if (siz > 1024) {
                    siz = 1024;
                }
                byte[] buf = new byte[siz];
                hd.getCopy(buf, 0, 0, siz);
                hd.getSkip(siz);
                if (sendFrame(typ, 0, curStr, buf)) {
                    return;
                }
                typ = typCont;
            }
            if (sendFrame(typ, flgHdrEnd, curStr, new byte[0])) {
                return;
            }
            if (len < 0) {
                for (;;) {
                    byte[] buf = new byte[1024];
                    int siz = userS.moreGet(buf, 0, buf.length);
                    if (siz < 1) {
                        break;
                    }
                    byte[] buf2 = new byte[siz];
                    bits.byteCopy(buf, 0, buf2, 0, buf2.length);
                    if (sendFrame(typData, 0, curStr, buf2)) {
                        return;
                    }
                }
                if (sendFrame(typData, flgStrEnd, curStr, new byte[0])) {
                    return;
                }
                return;
            }
            for (long pos = 0; pos < len;) {
                long siz = len - pos;
                if (siz > 1024) {
                    siz = 1024;
                }
                byte[] buf = new byte[(int) siz];
                if (userS.moreGet(buf, 0, buf.length) != buf.length) {
                    return;
                }
                if (sendFrame(typData, 0, curStr, buf)) {
                    return;
                }
                pos += siz;
            }
            if (sendFrame(typData, flgStrEnd, curStr, new byte[0])) {
                return;
            }
        }
    }

    private void removePad(byte[] buf) {
        if ((frFlg & flgPadded) == 0) {
            return;
        }
        datSiz -= buf[datOfs];
        datOfs++;
    }

    /**
     * receiver worker
     */
    protected void workerRx() {
        if (debugger.secWebsockTraf) {
            logger.debug("rx started");
        }
        for (;;) {
            byte[] buf = recvFrame();
            if (buf == null) {
                return;
            }
            datOfs = 0;
            datSiz = buf.length;
            switch (frTyp) {
                case typData:
                    if (frStr != curStr) {
                        sendRst(frStr);
                        break;
                    }
                    removePad(buf);
                    datSiz -= datOfs;
                    if (datSiz < 0) {
                        return;
                    }
                    if (userS.morePut(buf, datOfs, datSiz) != datSiz) {
                        return;
                    }
                    sendWindow(0, datSiz);
                    sendWindow(curStr, datSiz);
                    if ((frFlg & flgStrEnd) == 0) {
                        break;
                    }
                    if (clsReq) {
                        return;
                    }
                    break;
                case typSett:
                    if ((frFlg & flgAck) != 0) {
                        break;
                    }
                    sendFrame(typSett, flgAck, 0, new byte[0]);
                    break;
                case typHdrs:
                    removePad(buf);
                    if ((frFlg & flgPrio) != 0) {
                        datOfs += 5;
                    }
                    datSiz -= datOfs;
                    if (datSiz < 0) {
                        return;
                    }
                    packHolder hdb = new packHolder(true, true);
                    hdb.putCopy(buf, datOfs, 0, datSiz);
                    hdb.putSkip(datSiz);
                    hdb.merge2end();
                    for (;;) {
                        if ((frFlg & flgHdrEnd) != 0) {
                            break;
                        }
                        buf = recvFrame();
                        if (buf == null) {
                            return;
                        }
                        if (frTyp != typCont) {
                            return;
                        }
                        hdb.putCopy(buf, 0, 0, buf.length);
                        hdb.putSkip(buf.length);
                        hdb.merge2end();
                    }
                    List<String> hdt = hpackDecode(hdb, !client);
                    if (hdt == null) {
                        return;
                    }
                    for (int i = 0; i < hdt.size(); i++) {
                        userS.linePut(hdt.get(i));
                    }
                    curStr = frStr;
                    continue;
                case typPing:
                    if ((frFlg & flgAck) != 0) {
                        break;
                    }
                    sendFrame(typPing, flgAck, 0, buf);
                    break;
                case typPush:
                    removePad(buf);
                    datSiz -= datOfs;
                    if (datSiz < 0) {
                        return;
                    }
                    sendRst(bits.msbGetD(buf, datOfs));
                    for (;;) {
                        if ((frFlg & flgHdrEnd) != 0) {
                            break;
                        }
                        buf = recvFrame();
                        if (buf == null) {
                            return;
                        }
                        if (frTyp != typCont) {
                            return;
                        }
                    }
                    break;
                case typRst:
                    if (frStr != curStr) {
                        break;
                    }
                    return;
                case typWin:
                    break;
                case typGoaway:
                    break;
                case typPrio:
                    break;
                case typCont:
                    return;
                default:
                    break;
            }
        }
    }

}

class secHttp2huf {

    public final int asc;

    public final int val;

    public final int siz;

    public secHttp2huf(int a, int v, int s) {
        asc = a;
        val = v;
        siz = s;
    }

    public String toString() {
        return bits.toHexD(val) + "/" + siz + " " + bits.toHexB(asc);
    }

}

class secHttp2rx implements Runnable {

    private secHttp2 lower;

    public secHttp2rx(secHttp2 parent) {
        lower = parent;
        logger.startThread(this);
    }

    public void run() {
        try {
            lower.workerRx();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.workerStop();
        if (debugger.secHttp2traf) {
            logger.debug("rx stopped");
        }
    }

}

class secHttp2tx implements Runnable {

    private secHttp2 lower;

    public secHttp2tx(secHttp2 parent) {
        lower = parent;
        logger.startThread(this);
    }

    public void run() {
        try {
            lower.workerTx();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.workerStop();
        if (debugger.secHttp2traf) {
            logger.debug("tx stopped");
        }
    }

}
