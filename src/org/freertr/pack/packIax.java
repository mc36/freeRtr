package org.freertr.pack;

import org.freertr.pipe.pipeSide;
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
    public final static int ctr_ring = 3;

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
    public final static int icm_new = 1;

    /**
     * ping
     */
    public final static int icm_pin = 2;

    /**
     * pong
     */
    public final static int icm_pon = 3;

    /**
     * ack
     */
    public final static int icm_ack = 4;

    /**
     * hangup
     */
    public final static int icm_hup = 5;

    /**
     * reject
     */
    public final static int icm_rej = 6;

    /**
     * accept
     */
    public final static int icm_acc = 7;

    /**
     * authentication request
     */
    public final static int icm_arq = 8;

    /**
     * authentication reply
     */
    public final static int icm_arp = 9;

    /**
     * invalid message
     */
    public final static int icm_inv = 10;

    /**
     * lag request
     */
    public final static int icm_lrq = 11;

    /**
     * lag reply
     */
    public final static int icm_lrp = 12;

    /**
     * registration request
     */
    public final static int icm_rrq = 13;

    /**
     * registration authentication required
     */
    public final static int icm_rau = 14;

    /**
     * registration accepted
     */
    public final static int icm_rak = 15;

    /**
     * registration rejected
     */
    public final static int icm_rrj = 16;

    /**
     * registration release
     */
    public final static int icm_rrl = 17;

    /**
     * voice before valid first full frame
     */
    public final static int icm_vnk = 18;

    /**
     * dialplan request
     */
    public final static int icm_drq = 19;

    /**
     * dialplan reply
     */
    public final static int icm_drp = 20;

    /**
     * dial on channel
     */
    public final static int icm_dia = 21;

    /**
     * transfer request
     */
    public final static int icm_trq = 22;

    /**
     * transfer connect
     */
    public final static int icm_tcn = 23;

    /**
     * transfer accepted
     */
    public final static int icm_tac = 24;

    /**
     * transfer ready
     */
    public final static int icm_try = 25;

    /**
     * transfer release
     */
    public final static int icm_trl = 26;

    /**
     * transfer reject
     */
    public final static int icm_trj = 27;

    /**
     * stop transmission
     */
    public final static int icm_que = 28;

    /**
     * resume transmission
     */
    public final static int icm_uqu = 29;

    /**
     * ping but does not require an open connection
     */
    public final static int icm_pok = 30;

    /**
     * paging description
     */
    public final static int icm_pag = 31;

    /**
     * message waiting indicator
     */
    public final static int icm_mwi = 32;

    /**
     * unsupported message received
     */
    public final static int icm_uns = 33;

    /**
     * request remote transfer
     */
    public final static int icm_rtq = 34;

    /**
     * provision device
     */
    public final static int icm_prv = 35;

    /**
     * firmware download
     */
    public final static int icm_fdl = 36;

    /**
     * firmware data
     */
    public final static int icm_fdt = 37;

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
    public final static int iet_fmt = 9;

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
        tid = 0;
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
        int i = pck.pipeRecv(pipe, 0, -1, 143);
        if (i < 1) {
            return i;
        }
        i = pck.msbGetW(0); // f src
        sid = i & 0x7fff;
        if ((i & 0x8000) == 0) { // mini
            times = pck.msbGetW(2); // timestamp
            pck.getSkip(4);
            return pck.dataSize();
        }
        i = pck.msbGetW(2); // r trg
        tid = i & 0x7fff;
        times = pck.msbGetD(4); // timestamp
        seqO = pck.getByte(8); // out seq
        seqI = pck.getByte(9); // in seq
        typ = pck.getByte(10); // frame type
        sub = pck.getByte(11); // frame subtype
        pck.getSkip(12);
        return pck.dataSize();
    }

    /**
     * dump the message
     *
     * @param dir direction
     */
    public void dump(String dir) {
        logger.debug(dir + ": " + typ + " " + sub);
    }

}
