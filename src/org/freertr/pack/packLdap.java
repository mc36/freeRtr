package org.freertr.pack;

import java.math.BigInteger;
import org.freertr.enc.encAsn1;
import org.freertr.pipe.pipeSide;

/**
 * lightweight directory access protocol (rfc4511) packet
 *
 * @author matecsaba
 */
public class packLdap {

    /**
     * port number
     */
    public final static int port = 389;

    /**
     * header size
     */
    public final static int size = 6;

    /**
     * bind request
     */
    public final static int tpBindReq = 0;

    /**
     * bind reply
     */
    public final static int tpBindRep = 1;

    /**
     * unbind request
     */
    public final static int tpUnbind = 2;

    /**
     * search request
     */
    public final static int tpFindReq = 3;

    /**
     * search entry
     */
    public final static int tpFindNtry = 4;

    /**
     * search done
     */
    public final static int tpFindDone = 5;

    /**
     * search reference
     */
    public final static int tpFindRef = 6;

    /**
     * modify request
     */
    public final static int tpModReq = 7;

    /**
     * modify reply
     */
    public final static int tpModRep = 8;

    /**
     * add request
     */
    public final static int tpAddReq = 9;

    /**
     * add reply
     */
    public final static int tpAddRep = 10;

    /**
     * delete request
     */
    public final static int tpDelReq = 11;

    /**
     * delete reply
     */
    public final static int tpDelRep = 12;

    /**
     * modify dn request
     */
    public final static int tpModnReq = 13;

    /**
     * modify dn reply
     */
    public final static int tpModnRep = 14;

    /**
     * compare request
     */
    public final static int tpCmpReq = 15;

    /**
     * compare reply
     */
    public final static int tpCmpRep = 16;

    /**
     * abandom request
     */
    public final static int tpAbdReq = 17;

    /**
     * extended request
     */
    public final static int tpExtReq = 18;

    /**
     * extended reply
     */
    public final static int tpExtRep = 19;

    /**
     * success
     */
    public final static int cdSucc = 0;

    /**
     * operation error
     */
    public final static int cdOper = 1;

    /**
     * protocol error
     */
    public final static int cdProto = 2;

    /**
     * time limit exceeded
     */
    public final static int cdTime = 3;

    /**
     * size limit exceeded
     */
    public final static int cdSize = 4;

    /**
     * compare false
     */
    public final static int cdCmpF = 5;

    /**
     * compare true
     */
    public final static int cdCmpT = 6;

    /**
     * unsupported method
     */
    public final static int cdUnsup = 7;

    /**
     * weak method
     */
    public final static int cdWeak = 8;

    /**
     * referral
     */
    public final static int cdRef = 10;

    /**
     * admin limit exceeded
     */
    public final static int cdAdmin = 11;

    /**
     * critical extension
     */
    public final static int cdExt = 12;

    /**
     * confidentality required
     */
    public final static int cdConf = 13;

    /**
     * sasl binding in progress
     */
    public final static int cdSasl = 14;

    /**
     * no such attribute
     */
    public final static int cdSuch = 16;

    /**
     * undefined attribute
     */
    public final static int cdUndef = 17;

    /**
     * inappropirate mathching
     */
    public final static int cdInapr = 18;

    /**
     * constraint violation
     */
    public final static int cdConst = 19;

    /**
     * attribute exists
     */
    public final static int cdExist = 20;

    /**
     * invalid attribute syntax
     */
    public final static int cdSyntx = 21;

    /**
     * no such object
     */
    public final static int cdObjN = 32;

    /**
     * alias problem
     */
    public final static int cdAlias = 33;

    /**
     * invalid dn syntax
     */
    public final static int cdDnSyn = 34;

    /**
     * alias dereferencing problem
     */
    public final static int cdDeref = 36;

    /**
     * inappropirate authentication
     */
    public final static int cdAuth = 48;

    /**
     * invalid credentinals
     */
    public final static int cdCred = 49;

    /**
     * insufficient access rights
     */
    public final static int cdRight = 50;

    /**
     * busy
     */
    public final static int cdBusy = 51;

    /**
     * unavailable
     */
    public final static int cdUnava = 52;

    /**
     * unwilling to perform
     */
    public final static int cdUnwil = 53;

    /**
     * loop detect
     */
    public final static int cdLoop = 54;

    /**
     * naming violation
     */
    public final static int cdNaming = 64;

    /**
     * object class violation
     */
    public final static int cdObjV = 65;

    /**
     * not allowed on non leaf
     */
    public final static int cdNoLeaf = 66;

    /**
     * not allowed on rdn
     */
    public final static int cdNoRdn = 67;

    /**
     * entry already exists
     */
    public final static int cdAlredy = 68;

    /**
     * object class mods prohibited
     */
    public final static int cdObjP = 69;

    /**
     * affect multiple dsa
     */
    public final static int cdMult = 71;

    /**
     * current packet
     */
    public packHolder pack;

    /**
     * lower pipe
     */
    public pipeSide pipe;

    /**
     * message sequence ids
     */
    public int seq;

    /**
     * message type
     */
    public int typ;

    /**
     * result code
     */
    public int cod;

    /**
     * username
     */
    public String usr;

    /**
     * password
     */
    public String pwd;

    /**
     * create packet holder
     */
    public packLdap() {
        pack = new packHolder(true, true);
    }

    /**
     * create one header
     */
    public void packSend() {
        packHolder temp = new packHolder(true, true);
        encAsn1.writeBigInt(temp, new BigInteger("" + seq));
        encAsn1 a = new encAsn1();
        a.putEoc(pack);
        a.tag = typ;
        a.cls = 1;
        a.tagWrite(temp);
        temp.merge2end();
        pack.clear();
        encAsn1.writeSequence(pack, temp);
        pack.merge2beg();
        pack.pipeSend(pipe, 0, pack.dataSize(), 3);
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
        typ = pack.getByte(0); // type
        if (typ != 0x30) {
            return true;
        }
        typ = pack.getByte(1); // length
        seq = 2;
        if (typ >= 0x80) {
            seq += (typ & 0x7f);
            switch (seq) {
                case 3:
                    typ = pack.getByte(2);
                    break;
                case 4:
                    typ = pack.msbGetW(2);
                    break;
                case 5:
                    typ = pack.msbGetD(2) >>> 8;
                    break;
                case 6:
                    typ = pack.msbGetD(2);
                    break;
                default:
                    return true;
            }
        }
        seq += typ;
        seq -= size;
        if (pack.pipeRecv(pipe, size, seq, 144) != seq) {
            pipe.setClose();
            typ = -1;
            return true;
        }
        encAsn1 a = new encAsn1();
        if (a.tagRead(pack)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
            return true;
        }
        packHolder temp = a.getPack();
        BigInteger num = encAsn1.readBigInt(temp);
        if (num == null) {
            return true;
        }
        seq = num.intValue();
        if (a.tagRead(temp)) {
            return true;
        }
        if (!a.cnst) {
            return true;
        }
        typ = a.tag;
        pack = a.getPack();
        return false;
    }

    /**
     * dump this packet
     *
     * @return string
     */
    public String dump() {
        return "typ=" + typ + " seq=" + seq + " cod=" + cod + " usr=" + usr + " pwd=" + pwd;
    }

    /**
     * create bind request
     */
    public void createBindReq() {
        pack.clear();
        encAsn1.writeBigInt(pack, new BigInteger("3"));
        packHolder temp = new packHolder(true, true);
        temp.clear();
        byte[] buf = usr.getBytes();
        temp.putCopy(buf, 0, 0, buf.length);
        temp.putSkip(buf.length);
        encAsn1.writeOctString(pack, temp);
        temp.clear();
        buf = pwd.getBytes();
        temp.putCopy(buf, 0, 0, buf.length);
        temp.putSkip(buf.length);
        encAsn1 a = new encAsn1();
        a.putEoc(temp);
        a.cnst = false;
        a.tagWrite(pack);
        pack.merge2end();
        typ = tpBindReq;
    }

    /**
     * parse bind request
     *
     * @return true on error, false on success
     */
    public boolean parseBindReq() {
        if (typ != tpBindReq) {
            return true;
        }
        ///////////////////
        return false;
    }

    /**
     * create bind request
     */
    public void createBindRep() {

    

    ////////////////

    }

    /**
     * parse bind reply
     *
     * @return true on error, false on success
     */
    public boolean parseBindRep() {
        cod = -1;
        if (typ != tpBindRep) {
            return true;
        }
        encAsn1 a = new encAsn1();
        if (a.tagRead(pack)) {
            return true;
        }
        if (a.cnst || (a.tag != encAsn1.tagEnumerated)) {
            return true;
        }
        BigInteger num = a.getBigInt();
        if (num == null) {
            return true;
        }
        cod = num.intValue();
        return false;
    }

}
