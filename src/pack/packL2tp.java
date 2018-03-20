package pack;

import addr.addrIPv4;
import addr.addrType;
import util.bits;
import util.typLenVal;

/**
 * layer two tunneling protocol generic part
 *
 * @author matecsaba
 */
public abstract class packL2tp {

    /**
     * max value of tie breaker
     */
    public final static long maxTieBreak = 0xfffffffffffffff0L;

    /**
     * Start-Control-Connection-Request
     */
    public final static int typSCCRQ = 1;

    /**
     * Start-Control-Connection-Reply
     */
    public final static int typSCCRP = 2;

    /**
     * Start-Control-Connection-Connected
     */
    public final static int typSCCCN = 3;

    /**
     * Stop-Control-Connection-Notification
     */
    public final static int typSCCNO = 4;

    /**
     * Hello
     */
    public final static int typHELLO = 6;

    /**
     * Outgoing-Call-Request
     */
    public final static int typOCRQ = 7;

    /**
     * Outgoing-Call-Reply
     */
    public final static int typOCRP = 8;

    /**
     * Outgoing-Call-Connected
     */
    public final static int typOCCN = 9;

    /**
     * Incoming-Call-Request
     */
    public final static int typICRQ = 10;

    /**
     * Incoming-Call-Reply
     */
    public final static int typICRP = 11;

    /**
     * Incoming-Call-Connected
     */
    public final static int typICCN = 12;

    /**
     * Call-Disconnect-Notify
     */
    public final static int typCDN = 14;

    /**
     * WAN-Error-Notify
     */
    public final static int typWEN = 15;

    /**
     * Set-Link-Info
     */
    public final static int typSLI = 16;

    /**
     * Modem-Status
     */
    public final static int typMDM = 17;

    /**
     * Service-Relay-Request-Message
     */
    public final static int typSRRQ = 18;

    /**
     * Service-Relay-Reply-Message
     */
    public final static int typSRRP = 19;

    /**
     * Explicit-Acknowledgement
     */
    public final static int typACK = 20;

    /**
     * Failover-Session-Query
     */
    public final static int typFSQ = 21;

    /**
     * Failover-Session-Response
     */
    public final static int typFSR = 22;

    /**
     * Multicast-Session-Request
     */
    public final static int typMSRQ = 23;

    /**
     * Multicast-Session-Response
     */
    public final static int typMSRP = 24;

    /**
     * Multicast-Session-Establishment
     */
    public final static int typMSE = 25;

    /**
     * Multicast-Session-Information
     */
    public final static int typMSI = 26;

    /**
     * Multicast-Session-End-Notify
     */
    public final static int typMSEN = 27;

    /**
     * Connect-Speed-Update-Notification
     */
    public final static int typCSUN = 28;

    /**
     * Connect-Speed-Update-Request
     */
    public final static int typCSURQ = 29;

    /**
     * zero length block
     */
    public final static int typZLB = 0x10000;

    /**
     * message type
     */
    public final static int tlvMsgTyp = 0;

    /**
     * result code
     */
    public final static int tlvResCod = 1;

    /**
     * protocol version
     */
    public final static int tlvPrtVer = 2;

    /**
     * framing capability
     */
    public final static int tlvFramCap = 3;

    /**
     * bearer capability
     */
    public final static int tlvBearCap = 4;

    /**
     * tie breaker
     */
    public final static int tlvTieBrk = 5;

    /**
     * firmware revision
     */
    public final static int tlvFrmRev = 6;

    /**
     * host name
     */
    public final static int tlvHstNam = 7;

    /**
     * vendor name
     */
    public final static int tlvVendor = 8;

    /**
     * assigned tunnel id
     */
    public final static int tlvTunId = 9;

    /**
     * receive window size
     */
    public final static int tlvWinSiz = 10;

    /**
     * challenge
     */
    public final static int tlvChallen = 11;

    /**
     * cause code
     */
    public final static int tlvCausCod = 12;

    /**
     * challenge response
     */
    public final static int tlvResponse = 13;

    /**
     * assigned session id
     */
    public final static int tlvSesId = 14;

    /**
     * call serial number
     */
    public final static int tlvCallSer = 15;

    /**
     * minimum bps
     */
    public final static int tlvMinBps = 16;

    /**
     * maximum bps
     */
    public final static int tlvMaxBps = 17;

    /**
     * bearer type
     */
    public final static int tlvBearTyp = 18;

    /**
     * framing type
     */
    public final static int tlvFramTyp = 19;

    /**
     * called number
     */
    public final static int tlvCalled = 21;

    /**
     * calling number
     */
    public final static int tlvCalling = 22;

    /**
     * sub address
     */
    public final static int tlvSubAddr = 23;

    /**
     * tx connected speed
     */
    public final static int tlvTxSpeed = 24;

    /**
     * physical channel id
     */
    public final static int tlvPhyChan = 25;

    /**
     * initial received confreq
     */
    public final static int tlv1stRxCR = 26;

    /**
     * last sent confreq
     */
    public final static int tlvLstTxCR = 27;

    /**
     * last received confreq
     */
    public final static int tlvLstRxCR = 28;

    /**
     * proxy authen type
     */
    public final static int tlvAuthTyp = 29;

    /**
     * proxy authen name
     */
    public final static int tlvAuthNam = 30;

    /**
     * proxy authen challenge
     */
    public final static int tlvAuthChal = 31;

    /**
     * proxy authen id
     */
    public final static int tlvAuthID = 32;

    /**
     * proxy authen response
     */
    public final static int tlvAuthRep = 33;

    /**
     * call errors
     */
    public final static int tlvCallErr = 34;

    /**
     * async character map
     */
    public final static int tlvAccm = 35;

    /**
     * random vector
     */
    public final static int tlvRndVct = 36;

    /**
     * private group number
     */
    public final static int tlvPrvGrp = 37;

    /**
     * rx connected speed
     */
    public final static int tlvRxSpeed = 38;

    /**
     * sequencing required
     */
    public final static int tlvSeqReq = 39;

    /**
     * Rx Minimum BPS
     */
    public final static int tlvRxMinBps = 40;

    /**
     * Rx Maximum BPS
     */
    public final static int tlvRxMaxBps = 41;

    /**
     * Service Category
     */
    public final static int tlvServCat = 42;

    /**
     * Service Name
     */
    public final static int tlvServNam = 43;

    /**
     * Calling Sub-Address
     */
    public final static int tlvCallingSub = 44;

    /**
     * VPI/VCI Identifier
     */
    public final static int tlvVpiVciId = 45;

    /**
     * PPP Disconnect Cause Code
     */
    public final static int tlvPppDiscCause = 46;

    /**
     * CCDS AVP
     */
    public final static int tlvCcds = 47;

    /**
     * SDS AVP
     */
    public final static int tlvSds = 48;

    /**
     * LCP Want Options
     */
    public final static int tlvLcpWant = 49;

    /**
     * LCP Allow Options
     */
    public final static int tlvLcpAllow = 50;

    /**
     * LNS Last Sent LCP Confreq
     */
    public final static int tlvLnsLastTx = 51;

    /**
     * LNS Last Received LCP Confreq
     */
    public final static int tlvLnsLastRx = 52;

    /**
     * Modem On-Hold Capable AVP
     */
    public final static int tlvModemHoldCap = 53;

    /**
     * Modem On-Hold Status AVP
     */
    public final static int tlvModemHoldStat = 54;

    /**
     * PPPoE Relay AVP
     */
    public final static int tlvPppoeRel = 55;

    /**
     * PPPoE Relay Response Capability AVP
     */
    public final static int tlvPppoeRep = 56;

    /**
     * PPPoE Relay Forward Capability AVP
     */
    public final static int tlvPppoeFwd = 57;

    /**
     * Extended Vendor ID AVP
     */
    public final static int tlvExtVendor = 58;

    /**
     * Message Digest
     */
    public final static int tlvMsgDig = 59;

    /**
     * Router ID
     */
    public final static int tlvRtrID = 60;

    /**
     * Assigned Control Connection ID
     */
    public final static int tlvConnId = 61;

    /**
     * Pseudowire Capabilities List
     */
    public final static int tlvPwCap = 62;

    /**
     * Local Session ID
     */
    public final static int tlvLocSesId = 63;

    /**
     * Remote Session ID
     */
    public final static int tlvRemSesId = 64;

    /**
     * Assigned Cookie
     */
    public final static int tlvCookie = 65;

    /**
     * Remote End ID
     */
    public final static int tlvRemEndId = 66;

    /**
     * Application Code
     */
    public final static int tlvAppCod = 67;

    /**
     * Pseudowire Type
     */
    public final static int tlvPwTyp = 68;

    /**
     * L2-Specific Sublayer
     */
    public final static int tlvLay2sub = 69;

    /**
     * Data Sequencing
     */
    public final static int tlvSequence = 70;

    /**
     * Circuit Status
     */
    public final static int tlvCircStat = 71;

    /**
     * Preferred Language
     */
    public final static int tlvLang = 72;

    /**
     * Control Message Authentication Nonce
     */
    public final static int tlvMsgAuth = 73;

    /**
     * Tx Connect Speed
     */
    public final static int tlvTxConnSpd = 74;

    /**
     * Rx Connect Speed
     */
    public final static int tlvRxConnSpd = 75;

    /**
     * Failover Capability
     */
    public final static int tlvFailCap = 76;

    /**
     * Tunnel Recovery
     */
    public final static int tlvTunRec = 77;

    /**
     * Suggested Control Sequence
     */
    public final static int tlvSuggCtrlSeq = 78;

    /**
     * Failover Session State
     */
    public final static int tlvFailSesStat = 79;

    /**
     * Multicast Capability
     */
    public final static int tlvMultiCap = 80;

    /**
     * New Outgoing Sessions
     */
    public final static int tlvNewOutSess = 81;

    /**
     * New Outgoing Sessions Acknowledgement
     */
    public final static int tlvNewOutAck = 82;

    /**
     * Withdraw Outgoing Sessions
     */
    public final static int tlvWithOutSess = 83;

    /**
     * Multicast Packets Priority
     */
    public final static int tlvMultiPckPri = 84;

    /**
     * Frame-Relay Header Length
     */
    public final static int tlvFrHedLen = 85;

    /**
     * ATM Maximum Concatenated Cells AVP
     */
    public final static int tlvAtmMaxConcat = 86;

    /**
     * OAM Emulation Required AVP
     */
    public final static int tlvOamEmuReq = 87;

    /**
     * ATM Alarm Status AVP
     */
    public final static int tlvAtmAlarm = 88;

    /**
     * Attachment Group Identifier
     */
    public final static int tlvAttGrpId = 89;

    /**
     * Local End Identifier
     */
    public final static int tlvLocEndId = 90;

    /**
     * Interface Maximum Transmission Unit
     */
    public final static int tlvIntMTU = 91;

    /**
     * FCS Retention
     */
    public final static int tlvFcsRet = 92;

    /**
     * Tunnel Switching Aggregator ID AVP
     */
    public final static int tlvTunSwAggrId = 93;

    /**
     * Maximum Receive Unit (MRU) AVP
     */
    public final static int tlvMRU = 94;

    /**
     * Maximum Reassembled Receive Unit (MRRU) AVP
     */
    public final static int tlvMRRU = 95;

    /**
     * VCCV Capability AVP
     */
    public final static int tlvVccvCap = 96;

    /**
     * Connect Speed Update AVP
     */
    public final static int tlvConnSpdUpd = 97;

    /**
     * Connect Speed Update Enable AVP
     */
    public final static int tlvConnSpdEna = 98;

    /**
     * TDM Pseudowire AVP
     */
    public final static int tlvTdmPw = 99;

    /**
     * RTP AVP
     */
    public final static int tlvRtp = 100;

    /**
     * PW Switching Point AVP
     */
    public final static int tlvPwSwPnt = 101;

    /**
     * type length value handler
     */
    protected typLenVal tlv = new typLenVal(18, 30, 6, 10, 1, 6, 6, 1, 0, 1024, true);

    /**
     * message type
     */
    public int valMsgTyp = -1;

    /**
     * result code
     */
    public int valResCod = -1;

    /**
     * protocol version
     */
    public int valPrtVer = -1;

    /**
     * framing capability
     */
    public int valFramCap = -1;

    /**
     * bearer capability
     */
    public int valBearCap = -1;

    /**
     * tie breaker
     */
    public long valTieBrk = -1;

    /**
     * firmware revision
     */
    public int valFrmRev = -1;

    /**
     * host name
     */
    public String valHstNam = null;

    /**
     * vendor name
     */
    public String valVendor = null;

    /**
     * assigned tunnel id
     */
    public int valTunId = -1;

    /**
     * assigned session id
     */
    public int valSesId = -1;

    /**
     * receive window size
     */
    public int valWinSiz = -1;

    /**
     * call serial number
     */
    public int valCallSer = -1;

    /**
     * minimum bps
     */
    public int valMinBps = -1;

    /**
     * maximum bps
     */
    public int valMaxBps = -1;

    /**
     * bearer type
     */
    public int valBearTyp = -1;

    /**
     * framing type
     */
    public int valFramTyp = -1;

    /**
     * called number
     */
    public String valCalled = null;

    /**
     * calling number
     */
    public String valCalling = null;

    /**
     * sub address
     */
    public String valSubAddr = null;

    /**
     * tx connected speed
     */
    public int valTxSpeed = -1;

    /**
     * rx connected speed
     */
    public int valRxSpeed = -1;

    /**
     * physical channel id
     */
    public int valPhyChan = -1;

    /**
     * async character map
     */
    public int valAccm = -1;

    /**
     * sequencing required
     */
    public boolean valSeqReq = false;

    /**
     * Router ID
     */
    public addrIPv4 valRtrID = null;

    /**
     * Assigned Control Connection ID
     */
    public int valConnId = -1;

    /**
     * Pseudowire Capabilities List
     */
    public int[] valPwCap;

    /**
     * Local Session ID
     */
    public int valLocSesId = -1;

    /**
     * Remote Session ID
     */
    public int valRemSesId = -1;

    /**
     * Remote End ID
     */
    public String valRemEndId = null;

    /**
     * Pseudowire Type
     */
    public int valPwTyp = -1;

    /**
     * Circuit Status
     */
    public int valCircStat = -1;

    /**
     * challenge
     */
    public byte[] valChallen;

    /**
     * challenge response
     */
    public byte[] valResponse;

    /**
     * parse header
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public abstract boolean parseHeader(packHolder pck);

    /**
     * create header
     *
     * @param pck packet to use
     */
    public abstract void createHeader(packHolder pck);

    /**
     * dump header
     *
     * @return string
     */
    public abstract String dumpHeader();

    /**
     * dump current message
     *
     * @return string
     */
    public String dump() {
        return dumpHeader() + " | " + dumpTlvs();
    }

    /**
     * convert message type to string
     *
     * @param i message type
     * @return string
     */
    public String type2string(int i) {
        switch (i) {
            case typZLB:
                return "zero length block";
            case typSCCRQ:
                return "start request";
            case typSCCRP:
                return "start reply";
            case typSCCCN:
                return "start connected";
            case typSCCNO:
                return "stop notify";
            case typHELLO:
                return "hello";
            case typOCRQ:
                return "outgoing request";
            case typOCRP:
                return "outgoing reply";
            case typOCCN:
                return "outgoing connected";
            case typICRQ:
                return "incoming request";
            case typICRP:
                return "incoming reply";
            case typICCN:
                return "incoming connected";
            case typCDN:
                return "call disconnect";
            case typWEN:
                return "wan error";
            case typSLI:
                return "link info";
            case typMDM:
                return "modem status";
            case typSRRQ:
                return "relay request";
            case typSRRP:
                return "relay reply";
            case typACK:
                return "explicit acknowledge";
            case typFSQ:
                return "failover query";
            case typFSR:
                return "failover response";
            case typMSRQ:
                return "multicast request";
            case typMSRP:
                return "multicast response";
            case typMSE:
                return "multicast establish";
            case typMSI:
                return "multicast information";
            case typMSEN:
                return "multicast end notify";
            case typCSUN:
                return "speed notify";
            case typCSURQ:
                return "speed request";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert tlv type to string
     *
     * @param i tlv type
     * @return string
     */
    public String tlv2string(int i) {
        switch (i) {
            case tlvMsgTyp:
                return "message type";
            case tlvResCod:
                return "result code";
            case tlvPrtVer:
                return "protocol version";
            case tlvFramCap:
                return "framing capability";
            case tlvBearCap:
                return "bearer capability";
            case tlvTieBrk:
                return "tie breaker";
            case tlvFrmRev:
                return "firmware revision";
            case tlvHstNam:
                return "host name";
            case tlvVendor:
                return "vendor name";
            case tlvTunId:
                return "tunnel id";
            case tlvWinSiz:
                return "window size";
            case tlvChallen:
                return "challenge";
            case tlvCausCod:
                return "cause code";
            case tlvResponse:
                return "response";
            case tlvSesId:
                return "session id";
            case tlvCallSer:
                return "call serial";
            case tlvMinBps:
                return "minimum bps";
            case tlvMaxBps:
                return "maximum bps";
            case tlvBearTyp:
                return "bearer type";
            case tlvFramTyp:
                return "framing type";
            case tlvCalled:
                return "called number";
            case tlvCalling:
                return "calling number";
            case tlvSubAddr:
                return "sub address";
            case tlvTxSpeed:
                return "tx speed";
            case tlvPhyChan:
                return "channel id";
            case tlv1stRxCR:
                return "initial received confreq";
            case tlvLstTxCR:
                return "last sent confreq";
            case tlvLstRxCR:
                return "last received confreq";
            case tlvAuthTyp:
                return "proxy authen type";
            case tlvAuthNam:
                return "proxy authen name";
            case tlvAuthChal:
                return "proxy authen challenge";
            case tlvAuthID:
                return "proxy authen id";
            case tlvAuthRep:
                return "proxy authen response";
            case tlvCallErr:
                return "call errors";
            case tlvAccm:
                return "async character map";
            case tlvRndVct:
                return "random vector";
            case tlvPrvGrp:
                return "private group number";
            case tlvRxSpeed:
                return "rx speed";
            case tlvSeqReq:
                return "sequencing required";
            case tlvRxMinBps:
                return "Rx Minimum BPS";
            case tlvRxMaxBps:
                return "Rx Maximum BPS";
            case tlvServCat:
                return "Service Category";
            case tlvServNam:
                return "Service Name";
            case tlvCallingSub:
                return "Calling Sub-Address";
            case tlvVpiVciId:
                return "VPI/VCI Identifier";
            case tlvPppDiscCause:
                return "PPP Disconnect Cause Code";
            case tlvCcds:
                return "CCDS";
            case tlvSds:
                return "SDS";
            case tlvLcpWant:
                return "LCP Want Options";
            case tlvLcpAllow:
                return "LCP Allow Options";
            case tlvLnsLastTx:
                return "LNS Last Sent LCP Confreq";
            case tlvLnsLastRx:
                return "LNS Last Received LCP Confreq";
            case tlvModemHoldCap:
                return "Modem On-Hold Capable";
            case tlvModemHoldStat:
                return "Modem On-Hold Status";
            case tlvPppoeRel:
                return "PPPoE Relay";
            case tlvPppoeRep:
                return "PPPoE Relay Response Capability";
            case tlvPppoeFwd:
                return "PPPoE Relay Forward Capability";
            case tlvExtVendor:
                return "Extended Vendor ID";
            case tlvMsgDig:
                return "Message Digest";
            case tlvRtrID:
                return "Router ID";
            case tlvConnId:
                return "Assigned Control Connection ID";
            case tlvPwCap:
                return "Pseudowire Capabilities List";
            case tlvLocSesId:
                return "Local Session ID";
            case tlvRemSesId:
                return "Remote Session ID";
            case tlvCookie:
                return "Assigned Cookie";
            case tlvRemEndId:
                return "Remote End ID";
            case tlvAppCod:
                return "Application Code";
            case tlvPwTyp:
                return "Pseudowire Type";
            case tlvLay2sub:
                return "L2-Specific Sublayer";
            case tlvSequence:
                return "Data Sequencing";
            case tlvCircStat:
                return "Circuit Status";
            case tlvLang:
                return "Preferred Language";
            case tlvMsgAuth:
                return "Control Message Authentication Nonce";
            case tlvTxConnSpd:
                return "Tx Connect Speed";
            case tlvRxConnSpd:
                return "Rx Connect Speed";
            case tlvFailCap:
                return "Failover Capability";
            case tlvTunRec:
                return "Tunnel Recovery";
            case tlvSuggCtrlSeq:
                return "Suggested Control Sequence";
            case tlvFailSesStat:
                return "Failover Session State";
            case tlvMultiCap:
                return "Multicast Capability";
            case tlvNewOutSess:
                return "New Outgoing Sessions";
            case tlvNewOutAck:
                return "New Outgoing Sessions Acknowledgement";
            case tlvWithOutSess:
                return "Withdraw Outgoing Sessions";
            case tlvMultiPckPri:
                return "Multicast Packets Priority";
            case tlvFrHedLen:
                return "Frame-Relay Header Length";
            case tlvAtmMaxConcat:
                return "ATM Maximum Concatenated Cells";
            case tlvOamEmuReq:
                return "OAM Emulation Required";
            case tlvAtmAlarm:
                return "ATM Alarm Status";
            case tlvAttGrpId:
                return "Attachment Group Identifier";
            case tlvLocEndId:
                return "Local End Identifier";
            case tlvIntMTU:
                return "Interface Maximum Transmission Unit";
            case tlvFcsRet:
                return "FCS Retention";
            case tlvTunSwAggrId:
                return "Tunnel Switching Aggregator ID";
            case tlvMRU:
                return "Maximum Receive Unit";
            case tlvMRRU:
                return "Maximum Reassembled Receive Unit";
            case tlvVccvCap:
                return "VCCV Capability";
            case tlvConnSpdUpd:
                return "Connect Speed Update";
            case tlvConnSpdEna:
                return "Connect Speed Update Enable";
            case tlvTdmPw:
                return "TDM Pseudowire";
            case tlvRtp:
                return "RTP";
            case tlvPwSwPnt:
                return "PW Switching Point";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * dump current message
     *
     * @return string
     */
    public String dumpTlvs() {
        return "type=" + type2string(valMsgTyp) + " res=" + valResCod + " ver=" + valPrtVer + " framing=" + valFramCap + " chalen=" + bits.byteDump(valChallen, 0, -1) + " respon=" + bits.byteDump(valResponse, 0, -1)
                + " bearer=" + valBearCap + " tie=" + valTieBrk + " firm=" + valFrmRev + " vendor=" + valVendor + " host="
                + valHstNam + " tun=" + valTunId + " ses=" + valSesId + " win=" + valWinSiz + " call=" + valCallSer + " bps="
                + valMinBps + "/" + valMaxBps + " bearer=" + valBearTyp + " framing=" + valFramTyp + " called=" + valCalled
                + " calling=" + valCalling + " subadr=" + valSubAddr + " speed=" + valTxSpeed + "/" + valRxSpeed + " chan="
                + valPhyChan + " accm=" + valAccm + " seq=" + valSeqReq + " rtr=" + valRtrID + " conn=" + valConnId + " ses="
                + valLocSesId + "/" + valRemSesId + " remend=" + valRemEndId + " pwtyp=" + valPwTyp + " stat=" + valCircStat;
    }

    /**
     * parse contents
     *
     * @param pck packet to use
     */
    public void parseTLVs(packHolder pck) {
        if (pck.dataSize() < 1) {
            valMsgTyp = typZLB;
            return;
        }
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case tlvMsgTyp:
                    valMsgTyp = bits.msbGetW(tlv.valDat, 0);
                    break;
                case tlvResCod:
                    valResCod = bits.msbGetW(tlv.valDat, 0);
                    break;
                case tlvPrtVer:
                    valPrtVer = bits.msbGetW(tlv.valDat, 0);
                    break;
                case tlvFramCap:
                    valFramCap = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvBearCap:
                    valBearCap = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvTieBrk:
                    valTieBrk = bits.msbGetQ(tlv.valDat, 0);
                    break;
                case tlvFrmRev:
                    valFrmRev = bits.msbGetW(tlv.valDat, 0);
                    break;
                case tlvHstNam:
                    valHstNam = tlv.getStr();
                    break;
                case tlvVendor:
                    valVendor = tlv.getStr();
                    break;
                case tlvTunId:
                    valTunId = bits.msbGetW(tlv.valDat, 0);
                    break;
                case tlvSesId:
                    valSesId = bits.msbGetW(tlv.valDat, 0);
                    break;
                case tlvWinSiz:
                    valWinSiz = bits.msbGetW(tlv.valDat, 0);
                    break;
                case tlvCallSer:
                    valCallSer = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvMinBps:
                    valMinBps = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvMaxBps:
                    valMaxBps = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvBearTyp:
                    valBearTyp = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvFramTyp:
                    valFramTyp = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvCalled:
                    valCalled = tlv.getStr();
                    break;
                case tlvCalling:
                    valCalling = tlv.getStr();
                    break;
                case tlvSubAddr:
                    valSubAddr = tlv.getStr();
                    break;
                case tlvTxSpeed:
                    valTxSpeed = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvRxSpeed:
                    valRxSpeed = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvPhyChan:
                    valPhyChan = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvAccm:
                    valAccm = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvSeqReq:
                    valSeqReq = true;
                    break;
                case tlvRtrID:
                    valRtrID = new addrIPv4();
                    valRtrID.fromBuf(tlv.valDat, 0);
                    break;
                case tlvConnId:
                    valConnId = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvPwCap:
                    valPwCap = new int[tlv.valSiz / 2];
                    for (int i = 0; i < valPwCap.length; i++) {
                        valPwCap[i] = bits.msbGetW(tlv.valDat, i * 2);
                    }
                    break;
                case tlvLocSesId:
                    valLocSesId = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvRemSesId:
                    valRemSesId = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvRemEndId:
                    valRemEndId = tlv.getStr();
                    break;
                case tlvPwTyp:
                    valPwTyp = bits.msbGetW(tlv.valDat, 0);
                    break;
                case tlvCircStat:
                    valCircStat = bits.msbGetW(tlv.valDat, 0);
                    break;
                case tlvChallen:
                    valChallen = tlv.copyBytes();
                    break;
                case tlvResponse:
                    valResponse = tlv.copyBytes();
                    break;
            }
        }
    }

    private void createWord(packHolder pck, boolean mand, int typ, int val) {
        if (val == -1) {
            return;
        }
        byte[] buf = new byte[2];
        int[] bts = null;
        bits.msbPutW(buf, 0, val);
        if (mand) {
            bts = new int[1];
            bts[0] = 7;
        }
        tlv.putBytes(pck, bts, typ, buf);
    }

    private void createDouble(packHolder pck, boolean mand, int typ, int val) {
        if (val == -1) {
            return;
        }
        byte[] buf = new byte[4];
        int[] bts = null;
        bits.msbPutD(buf, 0, val);
        if (mand) {
            bts = new int[1];
            bts[0] = 7;
        }
        tlv.putBytes(pck, bts, typ, buf);
    }

    private void createQuad(packHolder pck, boolean mand, int typ, long val) {
        if (val == -1) {
            return;
        }
        byte[] buf = new byte[8];
        int[] bts = null;
        bits.msbPutQ(buf, 0, val);
        if (mand) {
            bts = new int[1];
            bts[0] = 7;
        }
        tlv.putBytes(pck, bts, typ, buf);
    }

    private void createStr(packHolder pck, boolean mand, int typ, String val) {
        if (val == null) {
            return;
        }
        int[] bts = null;
        if (mand) {
            bts = new int[1];
            bts[0] = 7;
        }
        tlv.putStr(pck, bts, typ, val);
    }

    private void createBin(packHolder pck, boolean mand, int typ, byte[] val) {
        if (val == null) {
            return;
        }
        int[] bts = null;
        if (mand) {
            bts = new int[1];
            bts[0] = 7;
        }
        tlv.putBytes(pck, bts, typ, val);
    }

    private void createBool(packHolder pck, boolean mand, int typ, boolean val) {
        if (!val) {
            return;
        }
        int[] bts = null;
        if (mand) {
            bts = new int[1];
            bts[0] = 7;
        }
        tlv.putBytes(pck, bts, typ, 0, null);
    }

    private void createAddr(packHolder pck, boolean mand, int typ, addrType val) {
        if (val == null) {
            return;
        }
        byte[] buf = new byte[val.getSize()];
        int[] bts = null;
        val.toBuffer(buf, 0);
        if (mand) {
            bts = new int[1];
            bts[0] = 7;
        }
        tlv.putBytes(pck, bts, typ, buf);
    }

    private void createList(packHolder pck, boolean mand, int typ, int siz, int[] val) {
        if (val == null) {
            return;
        }
        byte[] buf = new byte[siz * val.length];
        for (int i = 0; i < val.length; i++) {
            switch (siz) {
                case 1:
                    bits.putByte(buf, i * siz, val[i]);
                    break;
                case 2:
                    bits.msbPutW(buf, i * siz, val[i]);
                    break;
                case 4:
                    bits.msbPutD(buf, i * siz, val[i]);
                    break;
                default:
                    return;
            }
        }
        int[] bts = null;
        if (mand) {
            bts = new int[1];
            bts[0] = 7;
        }
        tlv.putBytes(pck, bts, typ, buf);
    }

    /**
     * create contents
     *
     * @param pck packet to use
     */
    public void createTLVs(packHolder pck) {
        createWord(pck, true, tlvMsgTyp, valMsgTyp);
        createWord(pck, false, tlvResCod, valResCod);
        createWord(pck, true, tlvPrtVer, valPrtVer);
        createDouble(pck, true, tlvFramCap, valFramCap);
        createDouble(pck, false, tlvBearCap, valBearCap);
        createQuad(pck, false, tlvTieBrk, valTieBrk);
        createWord(pck, false, tlvFrmRev, valFrmRev);
        createStr(pck, true, tlvHstNam, valHstNam);
        createWord(pck, true, tlvTunId, valTunId);
        createWord(pck, true, tlvSesId, valSesId);
        createWord(pck, true, tlvWinSiz, valWinSiz);
        createDouble(pck, true, tlvCallSer, valCallSer);
        createDouble(pck, false, tlvMinBps, valMinBps);
        createDouble(pck, false, tlvMaxBps, valMaxBps);
        createDouble(pck, true, tlvFramTyp, valFramTyp);
        createDouble(pck, false, tlvBearTyp, valBearTyp);
        createStr(pck, false, tlvCalled, valCalled);
        createStr(pck, false, tlvCalling, valCalling);
        createStr(pck, false, tlvSubAddr, valSubAddr);
        createDouble(pck, false, tlvTxSpeed, valTxSpeed);
        createDouble(pck, false, tlvRxSpeed, valRxSpeed);
        createDouble(pck, false, tlvPhyChan, valPhyChan);
        createDouble(pck, false, tlvAccm, valAccm);
        createBool(pck, false, tlvSeqReq, valSeqReq);
        createAddr(pck, false, tlvRtrID, valRtrID);
        createDouble(pck, false, tlvConnId, valConnId);
        createList(pck, false, tlvPwCap, 2, valPwCap);
        createDouble(pck, false, tlvLocSesId, valLocSesId);
        createDouble(pck, false, tlvRemSesId, valRemSesId);
        createStr(pck, false, tlvRemEndId, valRemEndId);
        createWord(pck, false, tlvPwTyp, valPwTyp);
        createWord(pck, false, tlvCircStat, valCircStat);
        createBin(pck, false, tlvChallen, valChallen);
        createBin(pck, false, tlvResponse, valResponse);
        pck.merge2beg();
    }

}
