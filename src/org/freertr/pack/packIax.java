package org.freertr.pack;

import org.freertr.enc.encTlv;
import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * inter asterisk exchange (rfc5456) packet
 *
 * @author matecsaba
 */
public class packIax {

    /**
     * port number
     */
    public final static int port = 4569;

    /**
     * dtmf digit, subclass is the digit
     */
    public final static int typ_dtm = 1;

    /**
     * voice data
     */
    public final static int typ_voc = 2;

    /**
     * video frame
     */
    public final static int typ_vid = 3;

    /**
     * control frame
     */
    public final static int typ_ctr = 4;

    /**
     * empty frame
     */
    public final static int typ_nul = 5;

    /**
     * iax frame
     */
    public final static int typ_iax = 6;

    /**
     * text message
     */
    public final static int typ_txt = 7;

    /**
     * image frame
     */
    public final static int typ_img = 8;

    /**
     * html frame
     */
    public final static int typ_htm = 9;

    /**
     * comfort noise frame, subclass is level
     */
    public final static int typ_cng = 10;

    /**
     * other end has hungup
     */
    public final static int ctr_hng = 1;

    /**
     * local ring
     */
    public final static int ctr_rng = 2;

    /**
     * remote end is ringing
     */
    public final static int ctr_rig = 3;

    /**
     * remote end has answered
     */
    public final static int ctr_ans = 4;

    /**
     * remote end is busy
     */
    public final static int ctr_bsy = 5;

    /**
     * make it go off hook
     */
    public final static int ctr_hok = 6;

    /**
     * line is off hook
     */
    public final static int ctr_off = 7;

    /**
     * congestion
     */
    public final static int ctr_cng = 8;

    /**
     * flash hook
     */
    public final static int ctr_fls = 9;

    /**
     * wink
     */
    public final static int ctr_wnk = 10;

    /**
     * set an option
     */
    public final static int ctr_opt = 11;

    /**
     * key radio
     */
    public final static int ctr_key = 12;

    /**
     * unkey radio
     */
    public final static int ctr_uky = 13;

    /**
     * call progress
     */
    public final static int ctr_prg = 14;

    /**
     * call proceed
     */
    public final static int ctr_prc = 15;

    /**
     * call hold
     */
    public final static int ctr_hld = 16;

    /**
     * call unhold
     */
    public final static int ctr_uhd = 17;

    /**
     * new call
     */
    public final static int iam_new = 1;

    /**
     * ping
     */
    public final static int iam_pin = 2;

    /**
     * pong
     */
    public final static int iam_pon = 3;

    /**
     * ack
     */
    public final static int iam_ack = 4;

    /**
     * hangup
     */
    public final static int iam_hup = 5;

    /**
     * reject
     */
    public final static int iam_rej = 6;

    /**
     * accept
     */
    public final static int iam_acc = 7;

    /**
     * authentication request
     */
    public final static int iam_arq = 8;

    /**
     * authentication reply
     */
    public final static int iam_arp = 9;

    /**
     * invalid message
     */
    public final static int iam_inv = 10;

    /**
     * lag request
     */
    public final static int iam_lrq = 11;

    /**
     * lag reply
     */
    public final static int iam_lrp = 12;

    /**
     * registration request
     */
    public final static int iam_rrq = 13;

    /**
     * registration authentication required
     */
    public final static int iam_rau = 14;

    /**
     * registration accepted
     */
    public final static int iam_rak = 15;

    /**
     * registration rejected
     */
    public final static int iam_rrj = 16;

    /**
     * registration release
     */
    public final static int iam_rrl = 17;

    /**
     * voice before valid first full frame
     */
    public final static int iam_vnk = 18;

    /**
     * dialplan request
     */
    public final static int iam_drq = 19;

    /**
     * dialplan reply
     */
    public final static int iam_drp = 20;

    /**
     * dial on channel
     */
    public final static int iam_dia = 21;

    /**
     * transfer request
     */
    public final static int iam_trq = 22;

    /**
     * transfer connect
     */
    public final static int iam_tcn = 23;

    /**
     * transfer accepted
     */
    public final static int iam_tac = 24;

    /**
     * transfer ready
     */
    public final static int iam_try = 25;

    /**
     * transfer release
     */
    public final static int iam_trl = 26;

    /**
     * transfer reject
     */
    public final static int iam_trj = 27;

    /**
     * stop transmission
     */
    public final static int iam_que = 28;

    /**
     * resume transmission
     */
    public final static int iam_uqu = 29;

    /**
     * ping but does not require an open connection
     */
    public final static int iam_pok = 30;

    /**
     * paging description
     */
    public final static int iam_pag = 31;

    /**
     * message waiting indicator
     */
    public final static int iam_mwi = 32;

    /**
     * unsupported message received
     */
    public final static int iam_uns = 33;

    /**
     * request remote transfer
     */
    public final static int iam_rtq = 34;

    /**
     * provision device
     */
    public final static int iam_prv = 35;

    /**
     * firmware download
     */
    public final static int iam_fdl = 36;

    /**
     * firmware data
     */
    public final static int iam_fdt = 37;

    /**
     * called number - string
     */
    public final static int iet_cld = 1;

    /**
     * calling number - string
     */
    public final static int iet_cli = 2;

    /**
     * calling number ani for billing - string
     */
    public final static int iet_ani = 3;

    /**
     * name of caller - string
     */
    public final static int iet_nam = 4;

    /**
     * context for number - string
     */
    public final static int iet_ctx = 5;

    /**
     * username - string
     */
    public final static int iet_usr = 6;

    /**
     * password - string
     */
    public final static int iet_pwd = 7;

    /**
     * codec capability - unsigned int
     */
    public final static int iet_cap = 8;

    /**
     * desired codec format - unsigned int
     */
    public final static int iet_dsr = 9;

    /**
     * desired language - string
     */
    public final static int iet_lng = 10;

    /**
     * protocol version - short
     */
    public final static int iet_ver = 11;

    /**
     * adsi capability - short
     */
    public final static int iet_ads = 12;

    /**
     * originally dialed dnid - string
     */
    public final static int iet_dni = 13;

    /**
     * authentication method(s) - short
     */
    public final static int iet_aut = 14;

    /**
     * challenge data - string
     */
    public final static int iet_chl = 15;

    /**
     * md5 challenge result - string
     */
    public final static int iet_md5 = 16;

    /**
     * RSA challenge result - string
     */
    public final static int iet_rsa = 17;

    /**
     * apparent address of peer - struct sockaddr_in
     */
    public final static int iet_apr = 18;

    /**
     * when to refresh registration - short
     */
    public final static int iet_frs = 19;

    /**
     * dialplan status - short
     */
    public final static int iet_dps = 20;

    /**
     * call number of peer - short
     */
    public final static int iet_cln = 21;

    /**
     * cause - string
     */
    public final static int iet_cse = 22;

    /**
     * unknown command - byte
     */
    public final static int iet_unk = 23;

    /**
     * how many messages waiting - short
     */
    public final static int iet_msc = 24;

    /**
     * request auto-answering -- none
     */
    public final static int iet_aan = 25;

    /**
     * request music on hold with quelch -- none or string
     */
    public final static int iet_moh = 26;

    /**
     * transfer request identifier -- int
     */
    public final static int iet_tri = 27;

    /**
     * referring dnis -- string
     */
    public final static int iet_rdn = 28;

    /**
     * provisioning info
     */
    public final static int iet_prv = 29;

    /**
     * aes Provisioning info
     */
    public final static int iet_aes = 30;

    /**
     * date time
     */
    public final static int iet_dat = 31;

    /**
     * device type -- string
     */
    public final static int iet_dev = 32;

    /**
     * service identifier -- string
     */
    public final static int iet_svc = 33;

    /**
     * firmware revision -- u16
     */
    public final static int iet_fwr = 34;

    /**
     * firmware block description -- u32
     */
    public final static int iet_fwb = 35;

    /**
     * firmware block of data -- raw
     */
    public final static int iet_fwd = 36;

    /**
     * provisioning version (u32)
     */
    public final static int iet_pvr = 37;

    /**
     * calling presentation (u8)
     */
    public final static int iet_cpr = 38;

    /**
     * calling type of number (u8)
     */
    public final static int iet_ctn = 39;

    /**
     * calling transit network select (u16)
     */
    public final static int iet_ctr = 40;

    /**
     * supported sampling rates (u16)
     */
    public final static int iet_smp = 41;

    /**
     * hangup cause (u8)
     */
    public final static int iet_hcs = 42;

    /**
     * encryption format (u16)
     */
    public final static int iet_efr = 43;

    /**
     * encryption key (raw)
     */
    public final static int iet_eky = 44;

    /**
     * codec negotiation
     */
    public final static int iet_fpr = 45;

    /**
     * received jitter - u32
     */
    public final static int iet_jit = 46;

    /**
     * received loss - high byte loss pct, low 24 bits loss count
     */
    public final static int iet_lss = 47;

    /**
     * total received frames - u32
     */
    public final static int iet_tot = 48;

    /**
     * max playout delay for received frames - u16
     */
    public final static int iet_del = 49;

    /**
     * dropped frames - u32
     */
    public final static int iet_drp = 50;

    /**
     * frames received out of order u32
     */
    public final static int iet_ood = 51;

    /**
     * call number security token
     */
    public final static int iet_tok = 52;

    private final pipeSide pipe;

    /**
     * source call id
     */
    public int sid;

    /**
     * target call id
     */
    public int tid;

    /**
     * time stamp
     */
    public int times;

    /**
     * out sequence
     */
    public int seqO;

    /**
     * in sequence
     */
    public int seqI;

    /**
     * type
     */
    public int typ;

    /**
     * subtype
     */
    public int sub;

    /**
     * username
     */
    public String user;

    /**
     * refresh
     */
    public int frsh;

    /**
     * protocol
     */
    public int proto;

    /**
     * desired codec
     */
    public int codecD;

    /**
     * capable codec
     */
    public int codecC;

    /**
     * calling number
     */
    public String calling;

    /**
     * caller name
     */
    public String callnam;

    /**
     * called number
     */
    public String called;

    /**
     * original number
     */
    public String origin;

    /**
     * create packet handler
     *
     * @param lower lower layer to use
     */
    public packIax(pipeSide lower) {
        pipe = lower;
        clear();
    }

    /**
     * clear packet data
     */
    public void clear() {
        sid = 0;
        tid = -1;
        times = 0;
        seqO = 0;
        seqI = 0;
        typ = 0;
        sub = 0;
    }

    /**
     * bytes ready to receive
     *
     * @return bytes
     */
    public int ready2rx() {
        return pipe.ready2rx();
    }

    /**
     * check if closed
     *
     * @return status
     */
    public int isClosed() {
        return pipe.isClosed();
    }

    /**
     * receive one packet
     *
     * @param pck buffer to use
     * @return bytes received
     */
    public int recvPack(packHolder pck) {
        clear();
        pck.clear();
        int i = pck.pipeRecv(pipe, 0, -1, 143);
        if (i < 1) {
            return i;
        }
        i = pck.msbGetW(0); // f src
        sid = i & 0x7fff;
        if ((i & 0x8000) == 0) { // mini
            times = pck.msbGetW(2); // timestamp
            pck.getSkip(4);
            typ = typ_voc;
            return pck.dataSize();
        }
        i = pck.msbGetW(2); // r trg
        tid = i & 0x7fff;
        times = pck.msbGetD(4); // timestamp
        seqO = pck.getByte(8); // out seq
        seqI = pck.getByte(9); // in seq
        typ = pck.getByte(10); // type
        sub = pck.getByte(11); // subtype
        pck.getSkip(12);
        return pck.dataSize();
    }

    /**
     * send one packet
     *
     * @param pck buffer to use
     */
    public void sendPack(packHolder pck) {
        if (tid < 0) {
            pck.msbPutW(0, sid); // f src
            pck.msbPutW(2, times); // timestamp
            pck.putSkip(4);
            pck.merge2beg();
            pck.pipeSend(pipe, 0, pck.dataSize(), 2);
            return;
        }
        pck.msbPutW(0, sid | 0x8000); // f src
        pck.msbPutW(2, tid); // r trg
        pck.msbPutD(4, times); // timestamp
        pck.putByte(8, seqO); // out seq
        pck.putByte(9, seqI); // in seq
        pck.putByte(10, typ); // type
        pck.putByte(11, sub); // subtype
        pck.putSkip(12);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 2);
    }

    private static encTlv getTlv() {
        return new encTlv(0, 8, 8, 8, 1, 0, 2, 1, 0, 512, true);
    }

    /**
     * parse tlv
     *
     * @param pck buffer to use
     */
    public void parseTlvs(packHolder pck) {
        switch (typ) {
            case packIax.typ_iax:
            case packIax.typ_ctr:
                break;
            default:
                return;
        }
        user = null;
        frsh = 0;
        proto = 0;
        codecC = 0;
        codecD = 0;
        calling = null;
        callnam = null;
        called = null;
        origin = null;
        encTlv tlv = getTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case iet_usr:
                    user = tlv.getStr();
                    break;
                case iet_frs:
                    frsh = bits.msbGetW(tlv.valDat, 0);
                    break;
                case iet_cld:
                    called = tlv.getStr();
                    break;
                case iet_dni:
                    origin = tlv.getStr();
                    break;
                case iet_cli:
                    calling = tlv.getStr();
                    break;
                case iet_cln:
                    callnam = tlv.getStr();
                    break;
                case iet_ver:
                    proto = bits.msbGetW(tlv.valDat, 0);
                    break;
                case iet_cap:
                    codecC = bits.msbGetD(tlv.valDat, 0);
                    break;
                case iet_dsr:
                    codecD = bits.msbGetD(tlv.valDat, 0);
                    break;
            }
        }
    }

    /**
     * parse tlv
     *
     * @param pck buffer to use
     */
    public void placeTlvs(packHolder pck) {
        switch (typ) {
            case packIax.typ_iax:
            case packIax.typ_ctr:
                break;
            default:
                return;
        }
        encTlv tlv = getTlv();
        if (user != null) {
            tlv.putBytes(pck, iet_usr, user.getBytes());
        }
        if (proto > 0) {
            bits.msbPutW(tlv.valDat, 0, proto);
            tlv.putBytes(pck, iet_frs, 2, tlv.valDat);
        }
        if (frsh > 0) {
            bits.msbPutW(tlv.valDat, 0, frsh);
            tlv.putBytes(pck, iet_frs, 2, tlv.valDat);
        }
        if (codecC > 0) {
            bits.msbPutD(tlv.valDat, 0, codecC);
            tlv.putBytes(pck, iet_cap, 4, tlv.valDat);
        }
        if (codecD > 0) {
            bits.msbPutD(tlv.valDat, 0, codecD);
            tlv.putBytes(pck, iet_dsr, 4, tlv.valDat);
        }
        if (called != null) {
            tlv.putBytes(pck, iet_cld, called.getBytes());
        }
        if (calling != null) {
            tlv.putBytes(pck, iet_cli, calling.getBytes());
        }
        if (callnam != null) {
            tlv.putBytes(pck, iet_cln, callnam.getBytes());
        }
        if (origin != null) {
            tlv.putBytes(pck, iet_dni, origin.getBytes());
        }
        pck.merge2end();
    }

    /**
     * dump the message
     *
     * @param dir direction
     */
    public void dump(String dir) {
        String tp;
        String sb = null;
        switch (typ) {
            case typ_dtm:
                tp = "dtmf";
                break;
            case typ_voc:
                tp = "voice";
                break;
            case typ_vid:
                tp = "video";
                break;
            case typ_ctr:
                tp = "control";
                switch (sub) {
                    case ctr_hng:
                        sb = "hangup";
                        break;
                    case ctr_rng:
                        sb = "locRing";
                        break;
                    case ctr_rig:
                        sb = "remRing";
                        break;
                    case ctr_ans:
                        sb = "answer";
                        break;
                    case ctr_bsy:
                        sb = "busy";
                        break;
                    case ctr_hok:
                        sb = "makeHook";
                        break;
                    case ctr_off:
                        sb = "lineHook";
                        break;
                    case ctr_cng:
                        sb = "congestion";
                        break;
                    case ctr_fls:
                        sb = "flashHook";
                        break;
                    case ctr_wnk:
                        sb = "wink";
                        break;
                    case ctr_opt:
                        sb = "option";
                        break;
                    case ctr_key:
                        sb = "key";
                        break;
                    case ctr_uky:
                        sb = "unkey";
                        break;
                    case ctr_prg:
                        sb = "progress";
                        break;
                    case ctr_prc:
                        sb = "proceed";
                        break;
                    case ctr_hld:
                        sb = "hold";
                        break;
                    case ctr_uhd:
                        sb = "unhold";
                        break;
                    default:
                        sb = "unknown=" + sub;
                        break;
                }
                break;
            case typ_nul:
                tp = "empty";
                break;
            case typ_iax:
                tp = "iax";
                switch (sub) {
                    case iam_new:
                        sb = "new";
                        break;
                    case iam_pin:
                        sb = "ping";
                        break;
                    case iam_pon:
                        sb = "pong";
                        break;
                    case iam_ack:
                        sb = "ack";
                        break;
                    case iam_hup:
                        sb = "hangup";
                        break;
                    case iam_rej:
                        sb = "reject";
                        break;
                    case iam_acc:
                        sb = "accept";
                        break;
                    case iam_arq:
                        sb = "authReq";
                        break;
                    case iam_arp:
                        sb = "authRep";
                        break;
                    case iam_inv:
                        sb = "inval";
                        break;
                    case iam_lrq:
                        sb = "lagReq";
                        break;
                    case iam_lrp:
                        sb = "lagRep";
                        break;
                    case iam_rrq:
                        sb = "regReq";
                        break;
                    case iam_rau:
                        sb = "regAuth";
                        break;
                    case iam_rak:
                        sb = "regAcc";
                        break;
                    case iam_rrj:
                        sb = "regRej";
                        break;
                    case iam_rrl:
                        sb = "regRel";
                        break;
                    case iam_vnk:
                        sb = "vnak";
                        break;
                    case iam_drq:
                        sb = "planReq";
                        break;
                    case iam_drp:
                        sb = "planRep";
                        break;
                    case iam_dia:
                        sb = "dial";
                        break;
                    case iam_trq:
                        sb = "transReq";
                        break;
                    case iam_tcn:
                        sb = "transCon";
                        break;
                    case iam_tac:
                        sb = "transAcc";
                        break;
                    case iam_try:
                        sb = "transRdy";
                        break;
                    case iam_trl:
                        sb = "transRel";
                        break;
                    case iam_trj:
                        sb = "transRej";
                        break;
                    case iam_que:
                        sb = "pause";
                        break;
                    case iam_uqu:
                        sb = "resume";
                        break;
                    case iam_pok:
                        sb = "poke";
                        break;
                    case iam_pag:
                        sb = "paging";
                        break;
                    case iam_mwi:
                        sb = "waiting";
                        break;
                    case iam_uns:
                        sb = "unsupp";
                        break;
                    case iam_rtq:
                        sb = "transRem";
                        break;
                    case iam_prv:
                        sb = "provis";
                        break;
                    case iam_fdl:
                        sb = "firmDl";
                        break;
                    case iam_fdt:
                        sb = "firmDat";
                        break;
                    default:
                        sb = "unknown=" + sub;
                        break;
                }
                break;
            case typ_txt:
                tp = "text";
                break;
            case typ_img:
                tp = "image";
                break;
            case typ_htm:
                tp = "html";
                break;
            case typ_cng:
                tp = "noise";
                break;
            default:
                tp = "unknown=" + typ;
                break;
        }
        if (sb == null) {
            sb = "" + sub;
        }
        logger.debug(dir + ": typ=" + tp + " sub=" + sb + " sid=" + sid + " tid=" + tid + " seqi=" + seqI + " seqo=" + seqO);
    }

}
