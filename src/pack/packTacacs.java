package pack;

import pipe.pipeSide;
import util.bits;
import cry.cryHashMd5;

/**
 * terminal access controller access control system (rfc1492) packet
 *
 * @author matecsaba
 */
public class packTacacs {

    /**
     * port number
     */
    public final static int port = 49;

    /**
     * header size
     */
    public final static int size = 12;

    /**
     * cleartext transmission
     */
    public final static int flgClr = 0x1;

    /**
     * multiple session
     */
    public final static int flgMul = 0x4;

    /**
     * not echo
     */
    public final static int flgNech = 0x1;

    /**
     * abort
     */
    public final static int flgAbrt = 0x1;

    /**
     * authentication
     */
    public final static int typAuthen = 1;

    /**
     * authorization
     */
    public final static int typAuthor = 2;

    /**
     * accounting
     */
    public final static int typAccount = 3;

    /**
     * passed
     */
    public final static int sttPass = 1;

    /**
     * failed
     */
    public final static int sttFail = 2;

    /**
     * get data
     */
    public final static int sttGetDat = 3;

    /**
     * get username
     */
    public final static int sttGetUsr = 4;

    /**
     * get password
     */
    public final static int sttGetPwd = 5;

    /**
     * restart
     */
    public final static int sttGetRst = 6;

    /**
     * error
     */
    public final static int sttGetErr = 7;

    /**
     * follow
     */
    public final static int sttGetFlw = 33;

    /**
     * login
     */
    public final static int actLogin = 1;

    /**
     * change password
     */
    public final static int actChPwd = 2;

    /**
     * send password
     */
    public final static int actTxPwd = 3;

    /**
     * send authentication
     */
    public final static int actTxAut = 4;

    /**
     * not set
     */
    public final static int autyNotset = 0;

    /**
     * ascii login
     */
    public final static int autyAscii = 1;

    /**
     * password authentication protocol
     */
    public final static int autyPap = 2;

    /**
     * channenge-handshake authentication protocol
     */
    public final static int autyChap = 3;

    /**
     * arap
     */
    public final static int autyArap = 4;

    /**
     * ms-chap
     */
    public final static int autyMSchap = 5;

    /**
     * login
     */
    public final static int srvLogin = 1;

    /**
     * enable
     */
    public final static int srvEnable = 2;

    /**
     * ppp
     */
    public final static int srvPpp = 3;

    /**
     * arap
     */
    public final static int srvArap = 4;

    /**
     * pt
     */
    public final static int srvPt = 5;

    /**
     * rcmd
     */
    public final static int srvRcmd = 6;

    /**
     * x25
     */
    public final static int srvX25 = 7;

    /**
     * nasi
     */
    public final static int srvNasi = 8;

    /**
     * fwd proxy
     */
    public final static int srvPrxy = 9;

    /**
     * not set
     */
    public final static int metNotset = 0;

    /**
     * none
     */
    public final static int metNone = 1;

    /**
     * kerberos5
     */
    public final static int metKrb5 = 2;

    /**
     * line
     */
    public final static int metLine = 3;

    /**
     * enable
     */
    public final static int metEna = 4;

    /**
     * local
     */
    public final static int metLoc = 5;

    /**
     * tacacs
     */
    public final static int metTac = 6;

    /**
     * guest
     */
    public final static int metGuest = 8;

    /**
     * radius
     */
    public final static int metRad = 16;

    /**
     * kerberos4
     */
    public final static int metKrb4 = 17;

    /**
     * rcmd
     */
    public final static int metRcmd = 32;

    /**
     * passed add
     */
    public final static int staPassAdd = 0;

    /**
     * passed reply
     */
    public final static int staPassRep = 1;

    /**
     * failed
     */
    public final static int staFail = 16;

    /**
     * error
     */
    public final static int staError = 17;

    /**
     * follow
     */
    public final static int staFolow = 33;

    /**
     * current packet
     */
    public packHolder pack;

    /**
     * lower pipe
     */
    public pipeSide pipe;

    /**
     * shared secret
     */
    public String secret;

    /**
     * version
     */
    public int ver;

    /**
     * message type
     */
    public int typ;

    /**
     * sequence number
     */
    public int seq;

    /**
     * flags
     */
    public int flg;

    /**
     * session id
     */
    public int ses;

    /**
     * action
     */
    public int act;

    /**
     * privilege level
     */
    public int priv;

    /**
     * authentication type
     */
    public int auty;

    /**
     * service
     */
    public int srv;

    /**
     * username
     */
    public String usr;

    /**
     * nas port
     */
    public String prt;

    /**
     * remote address
     */
    public String adr;

    /**
     * additional data
     */
    public String dat;

    /**
     * arguments
     */
    public String[] arg;

    /**
     * create packet holder
     */
    public packTacacs() {
        pack = new packHolder(true, true);
    }

    private boolean packCrypt() {
        if (secret == null) {
            return (flg & flgClr) == 0;
        }
        if ((flg & flgClr) != 0) {
            return true;
        }
        byte[] prv = new byte[0];
        for (int p = 0; p < pack.dataSize(); p += prv.length) {
            cryHashMd5 m = new cryHashMd5();
            m.init();
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, ses);
            m.update(buf, 0, buf.length);
            buf = secret.getBytes();
            m.update(buf, 0, buf.length);
            buf[0] = (byte) ver;
            m.update(buf, 0, 1);
            buf[0] = (byte) seq;
            m.update(buf, 0, 1);
            m.update(prv, 0, prv.length);
            prv = m.finish();
            for (int o = 0; o < prv.length; o++) {
                pack.putByte(p + o, pack.getByte(p + o) ^ prv[o]);
            }
        }
        int i = pack.dataSize();
        pack.getSkip(i);
        pack.putSkip(i);
        pack.merge2beg();
        return false;
    }

    /**
     * parse one header
     *
     * @return false on success, true on error
     */
    public boolean packRecv() {
        typ = -1;
        pack.clear();
        if (pack.pipeRecv(pipe, 0, size, 144) != size) {
            pipe.setClose();
            return true;
        }
        ver = pack.getByte(0); // version
        if ((ver & 0xf0) != 0xc0) {
            return true;
        }
        typ = pack.getByte(1); // type of packet
        seq = pack.getByte(2); // sequence number
        flg = pack.getByte(3); // flags
        ses = pack.msbGetD(4);
        int len = pack.msbGetD(8);
        if (pack.pipeRecv(pipe, size, len, 144) != len) {
            pipe.setClose();
            typ = -1;
            return true;
        }
        pack.getSkip(size);
        if (packCrypt()) {
            pipe.setClose();
            typ = -1;
            return true;
        }
        return false;
    }

    /**
     * create one header
     */
    public void packSend() {
        pack.merge2beg();
        seq++;
        ver = 0xc0;
        flg = 0;
        if (secret == null) {
            flg |= flgClr;
        }
        packCrypt();
        pack.putByte(0, ver);
        pack.putByte(1, typ);
        pack.putByte(2, seq);
        pack.putByte(3, flg);
        pack.msbPutD(4, ses);
        pack.msbPutD(8, pack.dataSize());
        pack.putSkip(size);
        pack.merge2beg();
        pack.pipeSend(pipe, 0, pack.dataSize(), 3);
    }

    /**
     * dump this packet
     *
     * @return string
     */
    public String dump() {
        String a = "";
        if (arg != null) {
            for (int i = 0; i < arg.length; i++) {
                a += " arg" + i + "=" + arg[i];
            }
        }
        return "typ=" + typ + " seq=" + seq + " ses=" + ses + " act=" + act + " priv=" + priv + " autyp=" + auty + " srv="
                + srv + " usr=" + usr + " prt=" + prt + " adr=" + adr + " dat=" + dat + a;
    }

    /**
     * parse authentication start
     *
     * @return false on success, true on error
     */
    public boolean parseAuthenStrt() {
        if (typ != typAuthen) {
            return true;
        }
        act = pack.getByte(0);
        priv = pack.getByte(1);
        auty = pack.getByte(2);
        srv = pack.getByte(3);
        int usrL = pack.getByte(4);
        int prtL = pack.getByte(5);
        int adrL = pack.getByte(6);
        int datL = pack.getByte(7);
        pack.getSkip(8);
        usr = pack.getAsciiZ(0, usrL, -1);
        pack.getSkip(usrL);
        prt = pack.getAsciiZ(0, prtL, -1);
        pack.getSkip(prtL);
        adr = pack.getAsciiZ(0, adrL, -1);
        pack.getSkip(adrL);
        dat = pack.getAsciiZ(0, datL, -1);
        pack.getSkip(datL);
        return false;
    }

    /**
     * create authentication start
     */
    public void createAuthenStrt() {
        pack.clear();
        typ = typAuthen;
        pack.putByte(0, act);
        pack.putByte(1, priv);
        pack.putByte(2, auty);
        pack.putByte(3, srv);
        int usrL = usr.length();
        int prtL = prt.length();
        int adrL = adr.length();
        int datL = dat.length();
        pack.putByte(4, usrL);
        pack.putByte(5, prtL);
        pack.putByte(6, adrL);
        pack.putByte(7, datL);
        pack.putSkip(8);
        pack.putAsciiZ(0, usrL, usr, 0);
        pack.putSkip(usrL);
        pack.putAsciiZ(0, prtL, prt, 0);
        pack.putSkip(prtL);
        pack.putAsciiZ(0, adrL, adr, 0);
        pack.putSkip(adrL);
        pack.putAsciiZ(0, datL, dat, 0);
        pack.putSkip(datL);
    }

    /**
     * parse authentication reply
     *
     * @return false on success, true on error
     */
    public boolean parseAuthenRply() {
        if (typ != typAuthen) {
            return true;
        }
        act = pack.getByte(0);
        priv = pack.getByte(1);
        int usrL = pack.msbGetW(2);
        int datL = pack.msbGetW(4);
        pack.getSkip(6);
        usr = pack.getAsciiZ(0, usrL, -1);
        pack.getSkip(usrL);
        dat = pack.getAsciiZ(0, datL, -1);
        pack.getSkip(datL);
        return false;
    }

    /**
     * create authentication reply
     */
    public void createAuthenRply() {
        pack.clear();
        typ = typAuthen;
        pack.putByte(0, act);
        pack.putByte(1, priv);
        int usrL = usr.length();
        int datL = dat.length();
        pack.msbPutW(2, usrL);
        pack.msbPutW(4, datL);
        pack.putSkip(6);
        pack.putAsciiZ(0, usrL, usr, 0);
        pack.putSkip(usrL);
        pack.putAsciiZ(0, datL, dat, 0);
        pack.putSkip(datL);
    }

    /**
     * parse authentication continue
     *
     * @return false on success, true on error
     */
    public boolean parseAuthenCont() {
        if (typ != typAuthen) {
            return true;
        }
        int usrL = pack.msbGetW(0);
        int datL = pack.msbGetW(2);
        act = pack.getByte(4);
        pack.getSkip(5);
        usr = pack.getAsciiZ(0, usrL, -1);
        pack.getSkip(usrL);
        dat = pack.getAsciiZ(0, datL, -1);
        pack.getSkip(datL);
        return false;
    }

    /**
     * parse authentication continue
     */
    public void createAuthenCont() {
        pack.clear();
        typ = typAuthen;
        int usrL = usr.length();
        int datL = dat.length();
        pack.msbPutW(0, usrL);
        pack.msbPutW(2, datL);
        pack.putByte(4, act);
        pack.putSkip(5);
        pack.putAsciiZ(0, usrL, usr, 0);
        pack.putSkip(usrL);
        pack.putAsciiZ(0, datL, dat, 0);
        pack.putSkip(datL);
    }

    /**
     * parse authorization request
     *
     * @return false on success, true on error
     */
    public boolean parseAuthorReq() {
        if (typ != typAuthor) {
            return true;
        }
        act = pack.getByte(0);
        priv = pack.getByte(1);
        auty = pack.getByte(2);
        srv = pack.getByte(3);
        int usrL = pack.getByte(4);
        int prtL = pack.getByte(5);
        int adrL = pack.getByte(6);
        int args = pack.getByte(7);
        int ofs = 8 + args;
        usr = pack.getAsciiZ(ofs, usrL, -1);
        ofs += usrL;
        prt = pack.getAsciiZ(ofs, prtL, -1);
        ofs += prtL;
        adr = pack.getAsciiZ(ofs, adrL, -1);
        ofs += adrL;
        arg = new String[args];
        for (int i = 0; i < args; i++) {
            int o = pack.getByte(8 + i);
            arg[i] = pack.getAsciiZ(ofs, o, -1);
            ofs += o;
        }
        return false;
    }

    /**
     * parse authorization request
     */
    public void createAuthorReq() {
        pack.clear();
        typ = typAuthor;
        pack.putByte(0, act);
        pack.putByte(1, priv);
        pack.putByte(2, auty);
        pack.putByte(3, srv);
        int usrL = usr.length();
        int prtL = prt.length();
        int adrL = adr.length();
        pack.putByte(4, usrL);
        pack.putByte(5, prtL);
        pack.putByte(6, adrL);
        pack.putByte(7, arg.length);
        pack.putSkip(8);
        for (int i = 0; i < arg.length; i++) {
            pack.putByte(i, arg[i].length());
        }
        pack.putSkip(arg.length);
        pack.putAsciiZ(0, usrL, usr, 0);
        pack.putSkip(usrL);
        pack.putAsciiZ(0, prtL, prt, 0);
        pack.putSkip(prtL);
        pack.putAsciiZ(0, adrL, adr, 0);
        pack.putSkip(adrL);
        for (int i = 0; i < arg.length; i++) {
            pack.putAsciiZ(0, arg[i].length(), arg[i], 0);
            pack.putSkip(arg[i].length());
        }
    }

    /**
     * parse authorization reply
     *
     * @return false on success, true on error
     */
    public boolean parseAuthorRep() {
        if (typ != typAuthor) {
            return true;
        }
        srv = pack.getByte(0);
        int args = pack.getByte(1);
        int usrL = pack.msbGetW(2);
        int adrL = pack.msbGetW(4);
        int ofs = 6 + args;
        usr = pack.getAsciiZ(ofs, usrL, -1);
        ofs += usrL;
        adr = pack.getAsciiZ(ofs, adrL, -1);
        ofs += adrL;
        arg = new String[args];
        for (int i = 0; i < args; i++) {
            int o = pack.getByte(6 + i);
            arg[i] = pack.getAsciiZ(ofs, o, -1);
            ofs += o;
        }
        return false;
    }

    /**
     * parse authorization reply
     */
    public void createAuthorRep() {
        pack.clear();
        typ = typAuthor;
        pack.putByte(0, srv);
        pack.putByte(1, arg.length);
        int usrL = usr.length();
        int adrL = adr.length();
        pack.msbPutW(2, usrL);
        pack.msbPutW(4, adrL);
        pack.putSkip(6);
        for (int i = 0; i < arg.length; i++) {
            pack.putByte(i, arg[i].length());
        }
        pack.putSkip(arg.length);
        pack.putAsciiZ(0, usrL, usr, 0);
        pack.putSkip(usrL);
        pack.putAsciiZ(0, adrL, adr, 0);
        pack.putSkip(adrL);
        for (int i = 0; i < arg.length; i++) {
            pack.putAsciiZ(0, arg[i].length(), arg[i], 0);
            pack.putSkip(arg[i].length());
        }
    }

}
