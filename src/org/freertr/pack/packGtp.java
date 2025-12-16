package org.freertr.pack;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgIfc;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;

/**
 * gprs tunneling protocol (3gpp29060) packet
 *
 * @author matecsaba
 */
public class packGtp {

    /**
     * create instance
     */
    public packGtp() {
    }

    /**
     * control port
     */
    public final static int portCtrl = 2123;

    /**
     * data port
     */
    public final static int portData = 2152;

    /**
     * size of header
     */
    public final static int size = 8;

    /**
     * flags value
     */
    public int flags;

    /**
     * message type
     */
    public int msgTyp;

    /**
     * tunnel endpoint id
     */
    public int tunId;

    /**
     * sequence number
     */
    public int seqNum;

    /**
     * n-pdu number
     */
    public int npduNum;

    /**
     * extension header type
     */
    public int extTyp;

    /**
     * cause code
     */
    public int valCause = -1;

    /**
     * client imsi
     */
    public String valIMSI;

    /**
     * reordering required
     */
    public int valReordReq = -1;

    /**
     * recovery counter
     */
    public int valRecovery = -1;

    /**
     * selection mode
     */
    public int valSelectMode = -1;

    /**
     * tunnel endpoint id data
     */
    public int valTeid1;

    /**
     * tunnel endpoint id control
     */
    public int valTeidCp;

    /**
     * teardown indicator
     */
    public int valTeardown = -1;

    /**
     * nsapi
     */
    public int valNSAPI = -1;

    /**
     * charging characteristics
     */
    public int valChargChar = -1;

    /**
     * charging id
     */
    public int valChargID;

    /**
     * framing mode
     */
    public int valEndUserAddr = -1;

    /**
     * framing address
     */
    public addrIPv4 valEndUserAddr4 = null;

    /**
     * framing address
     */
    public addrIPv6 valEndUserAddr6 = null;

    /**
     * apn name
     */
    public String valAccessPointName;

    /**
     * ggsn address
     */
    public addrIP valGSNaddr;

    /**
     * client isdn
     */
    public String valMSISDN;

    /**
     * qos profile
     */
    public int valQOSpro;

    /**
     * client imei
     */
    public String valIMEI;

    /**
     * tlv parser
     */
    public encTlv tlv = new encTlv(0, 8, 8, 16, 1, 0, 3, 1, 0, 512, true);

    /**
     * protocol type
     */
    public final static int flgProt = 0x10;

    /**
     * extension header
     */
    public final static int flgExt = 0x04;

    /**
     * sequence number
     */
    public final static int flgSeq = 0x02;

    /**
     * no flags set
     */
    public final static int flgNothing = 0x00;

    /**
     * n-pdu number
     */
    public final static int flgNpdu = 0x01;

    /**
     * ipv4
     */
    public final static int adrIp4 = 0xf121;

    /**
     * ipv6
     */
    public final static int adrIp6 = 0xf157;

    /**
     * ipv4 + ipv6
     */
    public final static int adrIp46 = 0xf18d;

    /**
     * ppp
     */
    public final static int adrPpp = 0xf001;

    /**
     * Echo Request
     */
    public final static int typEchoReq = 1;

    /**
     * Echo Response
     */
    public final static int typEchoRep = 2;

    /**
     * Version Not Supported
     */
    public final static int typUnsupp = 3;

    /**
     * Node Alive Request
     */
    public final static int typAliveReq = 4;

    /**
     * Node Alive Response
     */
    public final static int typAliveRep = 5;

    /**
     * Redirection Request
     */
    public final static int typRedirReq = 6;

    /**
     * Redirection Response
     */
    public final static int typRedirRep = 7;

    /**
     * Create PDP Context Request
     */
    public final static int typCreateReq = 16;

    /**
     * Create PDP Context Response
     */
    public final static int typCreateRep = 17;

    /**
     * Update PDP Context Request
     */
    public final static int typUpdateReq = 18;

    /**
     * Update PDP Context Response
     */
    public final static int typUpdateRep = 19;

    /**
     * Delete PDP Context Request
     */
    public final static int typDeleteReq = 20;

    /**
     * Delete PDP Context Response
     */
    public final static int typDeleteRep = 21;

    /**
     * Initiate PDP Context Activation Request
     */
    public final static int typInitReq = 22;

    /**
     * Initiate PDP Context Activation Response
     */
    public final static int typInitRep = 23;

    /**
     * Error Indication
     */
    public final static int typError = 26;

    /**
     * PDU Notification Request
     */
    public final static int typNotifyReq = 27;

    /**
     * PDU Notification Response
     */
    public final static int typNotifyRep = 28;

    /**
     * PDU Notification Reject Request
     */
    public final static int typNotifRejReq = 29;

    /**
     * PDU Notification Reject Response
     */
    public final static int typNotifRejRep = 30;

    /**
     * Supported Extension Headers Notification
     */
    public final static int typSupported = 31;

    /**
     * Send Routeing Information for GPRS Request
     */
    public final static int typRoutReq = 32;

    /**
     * Send Routeing Information for GPRS Response
     */
    public final static int typRoutRep = 33;

    /**
     * Failure Report Request
     */
    public final static int typFailReq = 34;

    /**
     * Failure Report Response
     */
    public final static int typFailRep = 35;

    /**
     * Note MS GPRS Present Request
     */
    public final static int typPresentReq = 36;

    /**
     * Note MS GPRS Present Response
     */
    public final static int typPresentRep = 37;

    /**
     * Identification Request
     */
    public final static int typIdentReq = 48;

    /**
     * Identification Response
     */
    public final static int typIdentRep = 49;

    /**
     * SGSN Context Request
     */
    public final static int typSgsnReq = 50;

    /**
     * SGSN Context Response
     */
    public final static int typSgsnRep = 51;

    /**
     * SGSN Context Acknowledge
     */
    public final static int typSgsnAck = 52;

    /**
     * Forward Relocation Request
     */
    public final static int typForwardReq = 53;

    /**
     * Forward Relocation Response
     */
    public final static int typForwardRep = 54;

    /**
     * Forward Relocation Complete
     */
    public final static int typForwardCmp = 55;

    /**
     * Relocation Cancel Request
     */
    public final static int typReloCanReq = 56;

    /**
     * Relocation Cancel Response
     */
    public final static int typRelocCanRep = 57;

    /**
     * Forward SRNS Context
     */
    public final static int typForwardSrnsReq = 58;

    /**
     * Forward Relocation Complete Acknowledge
     */
    public final static int typForwardAck = 59;

    /**
     * Forward SRNS Context Acknowledge
     */
    public final static int typForwardSrnsAck = 60;

    /**
     * RAN Information Relay
     */
    public final static int typRanRelay = 70;

    /**
     * MBMS Notification Request
     */
    public final static int typMbmsNotifReq = 96;

    /**
     * MBMS Notification Response
     */
    public final static int typMbmsNotifRep = 97;

    /**
     * MBMS Notification Reject Request
     */
    public final static int typMbmsRejReq = 98;

    /**
     * MBMS Notification Reject Response
     */
    public final static int typMbmsRejRep = 99;

    /**
     * Create MBMS Context Request
     */
    public final static int typCreateMbmsReq = 100;

    /**
     * Create MBMS Context Response
     */
    public final static int typCreateNbmsRep = 101;

    /**
     * Update MBMS Context Request
     */
    public final static int typUpdateMbmsReq = 102;

    /**
     * Update MBMS Context Response
     */
    public final static int typUpdateMbmsRep = 103;

    /**
     * Delete MBMS Context Request
     */
    public final static int typDeleteMbmsReq = 104;

    /**
     * Delete MBMS Context Response
     */
    public final static int typDeleteMbmsRep = 105;

    /**
     * MBMS Registration Request
     */
    public final static int typMbmsRegReq = 112;

    /**
     * MBMS Registration Response
     */
    public final static int typMbmsRegRep = 113;

    /**
     * MBMS De-Registration Request
     */
    public final static int typMbmsDelReq = 114;

    /**
     * MBMS De-Registration Response
     */
    public final static int typMbmsDelRep = 115;

    /**
     * MBMS Session Start Request
     */
    public final static int typMbmsStartReq = 116;

    /**
     * MBMS Session Start Response
     */
    public final static int typMbmsStartRep = 117;

    /**
     * MBMS Session Stop Request
     */
    public final static int typMbmsStopReq = 118;

    /**
     * MBMS Session Stop Response
     */
    public final static int typMbmsStopRep = 119;

    /**
     * MBMS Session Update Request
     */
    public final static int typMbmsUpdateReq = 120;

    /**
     * MBMS Session Update Response
     */
    public final static int typMbmsUpdateRep = 121;

    /**
     * MS Info Change Notification Request
     */
    public final static int typMsiChangeReq = 128;

    /**
     * MS Info Change Notification Response
     */
    public final static int typMsiChangeRep = 129;

    /**
     * Data Record Transfer Request
     */
    public final static int typDataRecReq = 240;

    /**
     * Data Record Transfer Response
     */
    public final static int typDataRecRep = 241;

    /**
     * End Marker
     */
    public final static int typEndMark = 254;

    /**
     * G-PDU
     */
    public final static int typGPDU = 255;

    /**
     * Cause
     */
    public final static int tlvCause = 1;

    /**
     * International Mobile Subscriber Identity (IMSI)
     */
    public final static int tlvIMSI = 2;

    /**
     * Routeing Area Identity (RAI)
     */
    public final static int tlvRAI = 3;

    /**
     * Temporary Logical Link Identity (TLLI)
     */
    public final static int tlvTLLI = 4;

    /**
     * Packet TMSI (P-TMSI)
     */
    public final static int tlvPTMSI = 5;

    /**
     * Reordering Required
     */
    public final static int tlvReordReq = 8;

    /**
     * Authentication Triplet
     */
    public final static int tlvAuthTrip = 9;

    /**
     * MAP Cause
     */
    public final static int tlvMAPcause = 11;

    /**
     * P-TMSI Signature
     */
    public final static int tlvPTMSIsig = 12;

    /**
     * MS Validated
     */
    public final static int tlvMSvalid = 13;

    /**
     * Recovery
     */
    public final static int tlvRecovery = 14;

    /**
     * Selection Mode
     */
    public final static int tlvSelectMode = 15;

    /**
     * Tunnel Endpoint Identifier Data I
     */
    public final static int tlvTeid1 = 16;

    /**
     * Tunnel Endpoint Identifier Control Plane
     */
    public final static int tlvTeidCp = 17;

    /**
     * Tunnel Endpoint Identifier Data II
     */
    public final static int tlvTeid2 = 18;

    /**
     * Teardown Ind
     */
    public final static int tlvTeardown = 19;

    /**
     * NSAPI
     */
    public final static int tlvNSAPI = 20;

    /**
     * RANAP Cause
     */
    public final static int tlvRANAPcause = 21;

    /**
     * RAB Context
     */
    public final static int tlvRABcontext = 22;

    /**
     * Radio Priority SMS
     */
    public final static int tlvRadioSMS = 23;

    /**
     * Radio Priority
     */
    public final static int tlvRadioPri = 24;

    /**
     * Packet Flow Id
     */
    public final static int tlvPackFlowId = 25;

    /**
     * Charging Characteristics
     */
    public final static int tlvChargChar = 26;

    /**
     * Trace Reference
     */
    public final static int tlvTraceRef = 27;

    /**
     * Trace Type
     */
    public final static int tlvTraceTyp = 28;

    /**
     * MS Not Reachable Reason
     */
    public final static int tlvMSunReach = 29;

    /**
     * Charging ID
     */
    public final static int tlvChargID = 127;

    /**
     * End User Address
     */
    public final static int tlvEndUserAddr = 128;

    /**
     * MM Context
     */
    public final static int tlvMMcontext = 129;

    /**
     * PDP Context
     */
    public final static int tlvPDPcontext = 130;

    /**
     * Access Point Name
     */
    public final static int tlvAccessPointName = 131;

    /**
     * Protocol Configuration Options
     */
    public final static int tlvProtCfgOpt = 132;

    /**
     * GSN Address
     */
    public final static int tlvGSNaddr = 133;

    /**
     * MS International PSTN/ISDN Number (MSISDN)
     */
    public final static int tlvMSISDN = 134;

    /**
     * Quality of Service Profile
     */
    public final static int tlvQOSpro = 135;

    /**
     * Authentication Quintuplet
     */
    public final static int tlvAuthQuin = 136;

    /**
     * Traffic Flow Template
     */
    public final static int tlvTrafFlowTemp = 137;

    /**
     * Target Identification
     */
    public final static int tlvTargetIdent = 138;

    /**
     * UTRAN Transparent Container
     */
    public final static int tlvUTRANtrans = 139;

    /**
     * RAB Setup Information
     */
    public final static int tlvRABsetup = 140;

    /**
     * Extension Header Type List
     */
    public final static int tlvExtHead = 141;

    /**
     * Trigger Id
     */
    public final static int tlvTrigId = 142;

    /**
     * OMC Identity
     */
    public final static int tlvOMCid = 143;

    /**
     * RAN Transparent Container
     */
    public final static int tlvRANtrans = 144;

    /**
     * PDP Context Prioritization
     */
    public final static int tlvPDPcontPri = 145;

    /**
     * Additional RAB Setup Information
     */
    public final static int tlvAddRABsetup = 146;

    /**
     * SGSN Number
     */
    public final static int tlvSGSNnum = 147;

    /**
     * Common Flags
     */
    public final static int tlvCommonFlags = 148;

    /**
     * APN Restriction
     */
    public final static int tlvAPNrestrict = 149;

    /**
     * Radio Priority LCS
     */
    public final static int tlvRadioLCS = 150;

    /**
     * RAT Type
     */
    public final static int tlvRATtype = 151;

    /**
     * User Location Information
     */
    public final static int tlvUserLocInfo = 152;

    /**
     * MS Time Zone
     */
    public final static int tlvMStimZon = 153;

    /**
     * IMEI(SV)
     */
    public final static int tlvIMEI = 154;

    /**
     * CAMEL Charging Information Container
     */
    public final static int tlvCAMELchargInfo = 155;

    /**
     * MBMS UE Context
     */
    public final static int tlvMBMSue = 156;

    /**
     * Temporary Mobile Group Identity (TMGI)
     */
    public final static int tlvTMGI = 157;

    /**
     * RIM Routing Address
     */
    public final static int tlvRIMroutAddr = 158;

    /**
     * MBMS Protocol Configuration Options
     */
    public final static int tlvMBMSprotCfg = 159;

    /**
     * MBMS Service Area
     */
    public final static int tlvMBMSservArea = 160;

    /**
     * Source RNC PDCP context info
     */
    public final static int tlvSrcRNCpdcp = 161;

    /**
     * Additional Trace Info
     */
    public final static int tlvAddTraceInfo = 162;

    /**
     * Hop Counter
     */
    public final static int tlvHopCount = 163;

    /**
     * Selected PLMN ID
     */
    public final static int tlvSelectPLMN = 164;

    /**
     * MBMS Session Identifier
     */
    public final static int tlvMBMSsessId = 165;

    /**
     * MBMS 2G/3G Indicator
     */
    public final static int tlvMBMSind = 166;

    /**
     * Enhanced NSAPI
     */
    public final static int tlvEnhNSAPI = 167;

    /**
     * MBMS Session Duration
     */
    public final static int tlvMBMSsessDur = 168;

    /**
     * Additional MBMS Trace Info
     */
    public final static int tlvAddMBMStrace = 169;

    /**
     * MBMS Session Repetition Number
     */
    public final static int tlvMBMSsessRepet = 170;

    /**
     * MBMS Time To Data Transfer
     */
    public final static int tlvMBMStime2data = 171;

    /**
     * BSS Container
     */
    public final static int tlvBSScont = 173;

    /**
     * Cell Identification
     */
    public final static int tlvCellId = 174;

    /**
     * PDU Numbers
     */
    public final static int tlvPDUnums = 175;

    /**
     * BSSGP Cause
     */
    public final static int tlvBSSGPcause = 176;

    /**
     * Required MBMS bearer capabilities
     */
    public final static int tlvReqMBMSbearCap = 177;

    /**
     * RIM Routing Address Discriminator
     */
    public final static int tlvRIMroutAddrDisc = 178;

    /**
     * List of set-up PFCs
     */
    public final static int tlvPFClist = 179;

    /**
     * PS Handover XID Parameters
     */
    public final static int tlvPShandXID = 180;

    /**
     * MS Info Change Reporting Action
     */
    public final static int tlvMSIchgReport = 181;

    /**
     * Direct Tunnel Flags
     */
    public final static int tlvDirTun = 182;

    /**
     * Correlation-ID
     */
    public final static int tlvCorrID = 183;

    /**
     * Bearer Control Mode
     */
    public final static int tlvBearCtrl = 184;

    /**
     * MBMS Flow Identifier
     */
    public final static int tlvMBMSflowId = 185;

    /**
     * MBMS IP Multicast Distribution
     */
    public final static int tlvMBMSmulticast = 186;

    /**
     * MBMS Distribution Acknowledgement
     */
    public final static int tlvMBMSdistrib = 187;

    /**
     * Reliable INTER RAT HANDOVER INFO
     */
    public final static int tlvRelRAThand = 188;

    /**
     * RFSP Index
     */
    public final static int tlvRFSPindex = 189;

    /**
     * Fully Qualified Domain Name (FQDN)
     */
    public final static int tlvFQDN = 190;

    /**
     * Evolved Allocation/Retention Priority I
     */
    public final static int tlvAllocRetPri1 = 191;

    /**
     * Evolved Allocation/Retention Priority II
     */
    public final static int tlvAllocRetPri2 = 192;

    /**
     * Extended Common Flags
     */
    public final static int tlvExtCommFlg = 193;

    /**
     * User CSG Information (UCI)
     */
    public final static int tlvUserCSGinfo = 194;

    /**
     * CSG Information Reporting Action
     */
    public final static int tlvCSGinfoReport = 195;

    /**
     * CSG ID
     */
    public final static int tlvCsgID = 196;

    /**
     * CSG Membership Indication (CMI)
     */
    public final static int tlvCSGmemberInd = 197;

    /**
     * Aggregate Maximum Bit Rate (AMBR)
     */
    public final static int tlvAggrMaxBitRate = 198;

    /**
     * UE Network Capability
     */
    public final static int tlvUEnetCapa = 199;

    /**
     * UE-AMBR
     */
    public final static int tlvUEambr = 200;

    /**
     * APN-AMBR with NSAPI
     */
    public final static int tlvAPNambr = 201;

    /**
     * Charging Gateway Address
     */
    public final static int tlvChargGwAddr = 251;

    /**
     * convert message type to string
     *
     * @param i message type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typEchoReq:
                return "echoRequest";
            case typEchoRep:
                return "echoResponse";
            case typUnsupp:
                return "versionNotSupported";
            case typAliveReq:
                return "nodeAliveRequest";
            case typAliveRep:
                return "nodeAliveResponse";
            case typRedirReq:
                return "redirectionRequest";
            case typRedirRep:
                return "redirectionResponse";
            case typCreateReq:
                return "createPDPcontextRequest";
            case typCreateRep:
                return "createPDPcontextResponse";
            case typUpdateReq:
                return "updatePDPcontextRequest";
            case typUpdateRep:
                return "updatePDPcontextResponse";
            case typDeleteReq:
                return "deletePDPcontextRequest";
            case typDeleteRep:
                return "deletePDPcontextResponse";
            case typInitReq:
                return "initiatePDPcontextActivationRequest";
            case typInitRep:
                return "initiatePDPcontextActivationResponse";
            case typError:
                return "errorIndication";
            case typNotifyReq:
                return "PDUnotificationRequest";
            case typNotifyRep:
                return "PDUnotificationResponse";
            case typNotifRejReq:
                return "PDUnotificationRejectRequest";
            case typNotifRejRep:
                return "PDUnotificationRejectResponse";
            case typSupported:
                return "supportedExtensionHeadersNotification";
            case typRoutReq:
                return "sendRouteingInformationforGPRSRequest";
            case typRoutRep:
                return "sendRouteingInformationforGPRSResponse";
            case typFailReq:
                return "failureReportRequest";
            case typFailRep:
                return "failureReportResponse";
            case typPresentReq:
                return "noteMSGPRSpresentRequest";
            case typPresentRep:
                return "noteMSGPRSpresentResponse";
            case typIdentReq:
                return "identificationRequest";
            case typIdentRep:
                return "identificationResponse";
            case typSgsnReq:
                return "SGSNcontextRequest";
            case typSgsnRep:
                return "SGSNcontextResponse";
            case typSgsnAck:
                return "SGSNcontextAcknowledge";
            case typForwardReq:
                return "forwardRelocationRequest";
            case typForwardRep:
                return "forwardRelocationResponse";
            case typForwardCmp:
                return "forwardRelocationComplete";
            case typReloCanReq:
                return "relocationCancelRequest";
            case typRelocCanRep:
                return "relocationCancelResponse";
            case typForwardSrnsReq:
                return "forwardSRNScontext";
            case typForwardAck:
                return "forwardRelocationCompleteAcknowledge";
            case typForwardSrnsAck:
                return "forwardSRNScontextAcknowledge";
            case typRanRelay:
                return "RANinformationRelay";
            case typMbmsNotifReq:
                return "MBMSnotificationRequest";
            case typMbmsNotifRep:
                return "MBMSnotificationResponse";
            case typMbmsRejReq:
                return "MBMSnotificationRejectRequest";
            case typMbmsRejRep:
                return "MBMSnotificationRejectResponse";
            case typCreateMbmsReq:
                return "createMBMScontextRequest";
            case typCreateNbmsRep:
                return "createMBMScontextResponse";
            case typUpdateMbmsReq:
                return "updateMBMScontextRequest";
            case typUpdateMbmsRep:
                return "updateMBMScontextResponse";
            case typDeleteMbmsReq:
                return "deleteMBMScontextRequest";
            case typDeleteMbmsRep:
                return "deleteMBMScontextResponse";
            case typMbmsRegReq:
                return "MBMSregistrationRequest";
            case typMbmsRegRep:
                return "MBMSregistrationResponse";
            case typMbmsDelReq:
                return "MBMSdeRegistrationRequest";
            case typMbmsDelRep:
                return "MBMSdeRegistrationResponse";
            case typMbmsStartReq:
                return "MBMSsessionStartRequest";
            case typMbmsStartRep:
                return "MBMSsessionStartResponse";
            case typMbmsStopReq:
                return "MBMSsessionStopRequest";
            case typMbmsStopRep:
                return "MBMSsessionStopResponse";
            case typMbmsUpdateReq:
                return "MBMSsessionUpdateRequest";
            case typMbmsUpdateRep:
                return "MBMSsessionUpdateResponse";
            case typMsiChangeReq:
                return "MSinfoChangeNotificationRequest";
            case typMsiChangeRep:
                return "MSinfoChangeNotificationResponse";
            case typDataRecReq:
                return "dataRecordTransferRequest";
            case typDataRecRep:
                return "dataRecordTransferResponse";
            case typEndMark:
                return "endMarker";
            case typGPDU:
                return "gpdu";
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
    public static String tlv2string(int i) {
        switch (i) {
            case tlvCause:
                return "cause";
            case tlvIMSI:
                return "internationalMobileSubscriberIdentity";
            case tlvRAI:
                return "routeingAreaIdentity";
            case tlvTLLI:
                return "temporaryLogicalLinkIdentity";
            case tlvPTMSI:
                return "packetTMSI";
            case tlvReordReq:
                return "reorderingRequired";
            case tlvAuthTrip:
                return "authenticationTriplet";
            case tlvMAPcause:
                return "MAPcause";
            case tlvPTMSIsig:
                return "PTMSIsignature";
            case tlvMSvalid:
                return "MSvalidated";
            case tlvRecovery:
                return "recovery";
            case tlvSelectMode:
                return "selectionMode";
            case tlvTeid1:
                return "tunnelEndpointIdentifierData1";
            case tlvTeidCp:
                return "tunnelEndpointIdentifierControlPlane";
            case tlvTeid2:
                return "tunnelEndpointIdentifierData2";
            case tlvTeardown:
                return "teardownInd";
            case tlvNSAPI:
                return "NSAPI";
            case tlvRANAPcause:
                return "RANAPcause";
            case tlvRABcontext:
                return "RABcontext";
            case tlvRadioSMS:
                return "radioPrioritySMS";
            case tlvRadioPri:
                return "radioPriority";
            case tlvPackFlowId:
                return "packetFlowId";
            case tlvChargChar:
                return "chargingCharacteristics";
            case tlvTraceRef:
                return "traceReference";
            case tlvTraceTyp:
                return "traceType";
            case tlvMSunReach:
                return "MSnotReachableReason";
            case tlvChargID:
                return "chargingID";
            case tlvEndUserAddr:
                return "endUserAddress";
            case tlvMMcontext:
                return "MMcontext";
            case tlvPDPcontext:
                return "PDPcontext";
            case tlvAccessPointName:
                return "accessPointName";
            case tlvProtCfgOpt:
                return "protocolConfigurationOptions";
            case tlvGSNaddr:
                return "GSNaddress";
            case tlvMSISDN:
                return "MSinternationalISDNnumber";
            case tlvQOSpro:
                return "qualityofServiceProfile";
            case tlvAuthQuin:
                return "authenticationQuintuplet";
            case tlvTrafFlowTemp:
                return "trafficFlowTemplate";
            case tlvTargetIdent:
                return "targetIdentification";
            case tlvUTRANtrans:
                return "UTRANtransparentContainer";
            case tlvRABsetup:
                return "RABsetupInformation";
            case tlvExtHead:
                return "extensionHeaderTypeList";
            case tlvTrigId:
                return "triggerId";
            case tlvOMCid:
                return "OMCidentity";
            case tlvRANtrans:
                return "RANtransparentContainer";
            case tlvPDPcontPri:
                return "PDPcontextPrioritization";
            case tlvAddRABsetup:
                return "additionalRABsetupInformation";
            case tlvSGSNnum:
                return "SGSNnumber";
            case tlvCommonFlags:
                return "commonFlags";
            case tlvAPNrestrict:
                return "APNrestriction";
            case tlvRadioLCS:
                return "radioPriorityLCS";
            case tlvRATtype:
                return "RATtype";
            case tlvUserLocInfo:
                return "userLocationInformation";
            case tlvMStimZon:
                return "MStimeZone";
            case tlvIMEI:
                return "IMEISV";
            case tlvCAMELchargInfo:
                return "CAMELchargingInformationContainer";
            case tlvMBMSue:
                return "MBMSueContext";
            case tlvTMGI:
                return "temporaryMobileGroupIdentity";
            case tlvRIMroutAddr:
                return "RIMroutingAddress";
            case tlvMBMSprotCfg:
                return "MBMSprotocolConfigurationOptions";
            case tlvMBMSservArea:
                return "MBMSserviceArea";
            case tlvSrcRNCpdcp:
                return "sourceRncPDCPcontextinfo";
            case tlvAddTraceInfo:
                return "additionalTraceInfo";
            case tlvHopCount:
                return "hopCounter";
            case tlvSelectPLMN:
                return "selectedPLMNID";
            case tlvMBMSsessId:
                return "MBMSsessionIdentifier";
            case tlvMBMSind:
                return "MBMS2G3Gindicator";
            case tlvEnhNSAPI:
                return "enhancedNSAPI";
            case tlvMBMSsessDur:
                return "MBMSsessionDuration";
            case tlvAddMBMStrace:
                return "additionalMBMSTraceInfo";
            case tlvMBMSsessRepet:
                return "MBMSsessionRepetitionNumber";
            case tlvMBMStime2data:
                return "MBMStimeToDataTransfer";
            case tlvBSScont:
                return "BSScontainer";
            case tlvCellId:
                return "cellIdentification";
            case tlvPDUnums:
                return "PDUnumbers";
            case tlvBSSGPcause:
                return "BSSGPcause";
            case tlvReqMBMSbearCap:
                return "requiredMBMSbearerCapabilities";
            case tlvRIMroutAddrDisc:
                return "RIMroutingAddressDiscriminator";
            case tlvPFClist:
                return "ListOfSetupPFCs";
            case tlvPShandXID:
                return "PShandoverXIDParameters";
            case tlvMSIchgReport:
                return "MSinfoChangeReportingAction";
            case tlvDirTun:
                return "directTunnelFlags";
            case tlvCorrID:
                return "correlationID";
            case tlvBearCtrl:
                return "bearerControlMode";
            case tlvMBMSflowId:
                return "MBMSflowIdentifier";
            case tlvMBMSmulticast:
                return "MBMSipMulticastDistribution";
            case tlvMBMSdistrib:
                return "MBMSdistributionAcknowledgement";
            case tlvRelRAThand:
                return "reliableInterRAThandoverInfo";
            case tlvRFSPindex:
                return "RFSPindex";
            case tlvFQDN:
                return "fullyQualifiedDomainName";
            case tlvAllocRetPri1:
                return "evolvedAllocation/RetentionPriorityI";
            case tlvAllocRetPri2:
                return "evolvedAllocation/RetentionPriorityII";
            case tlvExtCommFlg:
                return "extendedCommonFlags";
            case tlvUserCSGinfo:
                return "userCSGinformation";
            case tlvCSGinfoReport:
                return "CSGinformationReportingAction";
            case tlvCsgID:
                return "CSGid";
            case tlvCSGmemberInd:
                return "CSGmembershipIndication";
            case tlvAggrMaxBitRate:
                return "aggregateMaximumBitRate";
            case tlvUEnetCapa:
                return "UEnetworkCapability";
            case tlvUEambr:
                return "UEamber";
            case tlvAPNambr:
                return "APNamberWithNSAPI";
            case tlvChargGwAddr:
                return "chargingGatewayAddress";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert tlv type to string
     *
     * @param i tlv type
     * @return size of tlv, -1=unknown, -2=tlv
     */
    public static int tlv2size(int i) {
        if (i >= 0x80) {
            return -2;
        }
        switch (i) {
            case tlvCause:
                return 1;
            case tlvIMSI:
                return 8;
            case tlvRAI:
                return 6;
            case tlvTLLI:
                return 4;
            case tlvPTMSI:
                return 4;
            case tlvReordReq:
                return 1;
            case tlvAuthTrip:
                return 28;
            case tlvMAPcause:
                return 1;
            case tlvPTMSIsig:
                return 3;
            case tlvMSvalid:
                return 1;
            case tlvRecovery:
                return 1;
            case tlvSelectMode:
                return 1;
            case tlvTeid1:
                return 4;
            case tlvTeidCp:
                return 4;
            case tlvTeid2:
                return 5;
            case tlvTeardown:
                return 1;
            case tlvNSAPI:
                return 1;
            case tlvRANAPcause:
                return 1;
            case tlvRABcontext:
                return 9;
            case tlvRadioSMS:
                return 1;
            case tlvRadioPri:
                return 1;
            case tlvPackFlowId:
                return 2;
            case tlvChargChar:
                return 2;
            case tlvTraceRef:
                return 2;
            case tlvTraceTyp:
                return 2;
            case tlvMSunReach:
                return 1;
            case tlvChargID:
                return 4;
            default:
                return -1;
        }
    }

    /**
     * fill end user address
     *
     * @param cfger interface to test
     * @param peer use peer address
     */
    public void fillEndUserAddr(cfgIfc cfger, boolean peer) {
        if (cfger.ppp != null) {
            valEndUserAddr = packGtp.adrPpp; // mode
            return;
        }
        addrIPv4 adr4;
        addrIPv6 adr6;
        if (peer) {
            adr4 = cfger.ip4polA;
            adr6 = cfger.ip6polA;
        } else {
            adr4 = cfger.addr4;
            adr6 = cfger.addr6;
        }
        valEndUserAddr = packGtp.adrIp46; // ip46 mode
        if (adr4 == null) {
            valEndUserAddr = packGtp.adrIp6; // ip6 mode
        } else {
            valEndUserAddr4 = adr4.copyBytes();
        }
        if (adr6 == null) {
            valEndUserAddr = packGtp.adrIp4; // ip4 mode
        } else {
            valEndUserAddr6 = adr6.copyBytes();
        }
    }

    /**
     * dump this packet
     *
     * @return string
     */
    public String dump() {
        String s = "flag=" + flags + " type=" + type2string(msgTyp) + " tun=" + tunId + " seq=" + seqNum + " npdu=" + npduNum + " ext=" + extTyp;
        s += " cause=" + valCause + " imsi=" + valIMSI + " reorder=" + valReordReq + " recovery=" + valRecovery + " select=" + valSelectMode;
        s += " tunD=" + valTeid1 + " tunC=" + valTeidCp + " teardown=" + valTeardown + " nsapi=" + valNSAPI + " chrgChr=" + valChargChar;
        s += " chrgId=" + valChargID + " addr=" + valEndUserAddr + "," + valEndUserAddr4 + "," + valEndUserAddr6 + " apn=" + valAccessPointName;
        s += " gsn=" + valGSNaddr + " isdn=" + valMSISDN + " qos=" + valQOSpro + " imei=" + valIMEI;
        return s;
    }

    /**
     * parse header
     *
     * @param pck packet to use
     * @return false if successful, true if error happened
     */
    public boolean parseHeader(packHolder pck) {
        if (pck.dataSize() < size) {
            return true;
        }
        flags = pck.getByte(0); // flags + version
        if ((flags & 0xe0) != 0x20) {
            return true;
        }
        flags &= 0x1f;
        if ((flags & flgProt) == 0) {
            return true;
        }
        msgTyp = pck.getByte(1); // message type
        int len = pck.msbGetW(2); // message size
        tunId = pck.msbGetD(4); // tunnel endpoint id
        pck.getSkip(size);
        if (pck.dataSize() < len) {
            return true;
        }
        pck.setDataSize(len);
        if ((flags & (flgSeq | flgExt | flgNpdu)) != 0) {
            seqNum = pck.msbGetW(0); // sequence number
            npduNum = pck.getByte(2); // npdu number
            extTyp = pck.getByte(3); // extension header type
            pck.getSkip(4);
        }
        if ((flags & flgSeq) == 0) {
            seqNum = 0;
        }
        if ((flags & flgNpdu) == 0) {
            npduNum = 0;
        }
        if ((flags & flgExt) == 0) {
            extTyp = 0;
        }
        return false;
    }

    /**
     * parse extension header
     *
     * @param pck packet to use
     * @return false if successful, true if error happened
     */
    public boolean parseExtHdr(packHolder pck) {
        if (extTyp < 1) {
            return true;
        }
        tlv.valTyp = extTyp;
        int len = pck.getByte(0) * 4; // size of header
        if (pck.dataSize() < len) {
            return true;
        }
        extTyp = pck.getByte(len - 1);
        tlv.valSiz = len - 2;
        pck.getCopy(tlv.valDat, 0, 1, tlv.valSiz);
        pck.getSkip(len);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to use
     */
    public void createHeader(packHolder pck) {
        pck.merge2beg();
        if ((flags & (flgSeq | flgExt | flgNpdu)) != 0) {
            pck.msbPutW(0, seqNum); // sequence number
            pck.putByte(2, npduNum); // npdu number
            pck.putByte(3, extTyp); // extension header type
            pck.putSkip(4);
            pck.merge2beg();
        }
        pck.putByte(0, 0x20 | flgProt | (flags & 0x1f)); // version + flags
        pck.putByte(1, msgTyp); // message type
        pck.msbPutW(2, pck.dataSize()); // message size
        pck.msbPutD(4, tunId); // tunnel endpoint id
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * parse one tv/tlv
     *
     * @param pck packet to use
     * @return false if successful, true if error happened
     */
    public boolean parseTlv(packHolder pck) {
        if (pck.dataSize() < 1) {
            return true;
        }
        tlv.valTyp = pck.getByte(0);
        tlv.valSiz = tlv2size(tlv.valTyp);
        if (tlv.valSiz == -2) {
            return tlv.getBytes(pck);
        }
        if (tlv.valSiz < 0) {
            return true;
        }
        pck.getCopy(tlv.valDat, 0, 1, tlv.valSiz);
        pck.getSkip(tlv.valSiz + 1);
        return false;
    }

    /**
     * create one tlv
     *
     * @param pck packet to use
     */
    public void createTlv(packHolder pck) {
        int i = tlv2size(tlv.valTyp);
        if (i == -2) {
            tlv.putThis(pck);
            return;
        }
        pck.putByte(0, tlv.valTyp);
        pck.putCopy(tlv.valDat, 0, 1, i);
        pck.putSkip(i + 1);
    }

    /**
     * parse up number
     *
     * @return number read
     */
    public String parseNumber() {
        String s = "";
        for (int i = 0; i < tlv.valSiz; i++) {
            String a = bits.toHexB(bits.getByte(tlv.valDat, i));
            s += a.substring(1, 2) + a.substring(0, 1);
        }
        return s.toLowerCase().replaceAll("f", "");
    }

    /**
     * create one number
     *
     * @param s number to store
     * @return bytes written
     */
    public int createNumber(String s) {
        bits.byteFill(tlv.valDat, 0, 32, 0xff);
        if (s == null) {
            return 0;
        }
        int p = 0;
        for (;;) {
            if (s.length() < 2) {
                break;
            }
            int i = bits.str2num(s.substring(0, 1));
            int o = bits.str2num(s.substring(1, 2));
            s = s.substring(2, s.length());
            tlv.valDat[p++] = (byte) (i | (o << 4));
        }
        if (s.length() > 0) {
            tlv.valDat[p++] = (byte) (bits.str2num(s) | 0xf0);
        }
        return p;
    }

    /**
     * parse packet
     *
     * @param pck packet to read
     */
    public void parsePacket(packHolder pck) {
        for (;;) {
            if (parseTlv(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case tlvCause:
                    valCause = bits.getByte(tlv.valDat, 0);
                    break;
                case tlvIMSI:
                    valIMSI = parseNumber();
                    break;
                case tlvReordReq:
                    valReordReq = bits.getByte(tlv.valDat, 0);
                    break;
                case tlvRecovery:
                    valRecovery = bits.getByte(tlv.valDat, 0);
                    break;
                case tlvSelectMode:
                    valSelectMode = bits.getByte(tlv.valDat, 0);
                    break;
                case tlvTeid1:
                    valTeid1 = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvTeidCp:
                    valTeidCp = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvTeardown:
                    valTeardown = bits.getByte(tlv.valDat, 0);
                    break;
                case tlvNSAPI:
                    valNSAPI = bits.getByte(tlv.valDat, 0);
                    break;
                case tlvChargChar:
                    valChargChar = bits.msbGetW(tlv.valDat, 0);
                    break;
                case tlvChargID:
                    valChargID = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvEndUserAddr:
                    valEndUserAddr = bits.msbGetW(tlv.valDat, 0); // mode
                    switch (valEndUserAddr) {
                        case adrPpp:
                            break;
                        case adrIp4:
                            valEndUserAddr4 = new addrIPv4();
                            valEndUserAddr4.fromBuf(tlv.valDat, 2); // address
                            break;
                        case adrIp6:
                            valEndUserAddr6 = new addrIPv6();
                            valEndUserAddr6.fromBuf(tlv.valDat, 2);  // address
                            break;
                        case adrIp46:
                            valEndUserAddr4 = new addrIPv4();
                            valEndUserAddr6 = new addrIPv6();
                            valEndUserAddr4.fromBuf(tlv.valDat, 2);  // address
                            valEndUserAddr6.fromBuf(tlv.valDat, 6);  // address
                            break;
                    }
                    break;
                case tlvAccessPointName:
                    valAccessPointName = tlv.getStr();
                    valAccessPointName = valAccessPointName.substring(1, valAccessPointName.length());
                    break;
                case tlvGSNaddr:
                    valGSNaddr = new addrIP();
                    if (tlv.valSiz == addrIPv4.size) {
                        addrIPv4 a = new addrIPv4();
                        a.fromBuf(tlv.valDat, 0);
                        valGSNaddr.fromIPv4addr(a);
                    } else {
                        addrIPv6 a = new addrIPv6();
                        a.fromBuf(tlv.valDat, 0);
                        valGSNaddr.fromIPv6addr(a);
                    }
                    break;
                case tlvMSISDN:
                    valMSISDN = parseNumber();
                    valMSISDN = valMSISDN.substring(2, valMSISDN.length());
                    break;
                case tlvQOSpro:
                    valQOSpro = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvIMEI:
                    valIMEI = parseNumber();
                    break;
            }
        }
    }

    /**
     * create packet
     *
     * @return packet to send
     */
    public packHolder createPacket() {
        flags = packGtp.flgSeq;
        packHolder pck = new packHolder(true, true);
        if (valCause >= 0) {
            tlv.valDat[0] = (byte) valCause; // cause code
            tlv.valTyp = tlvCause;
            createTlv(pck);
        }
        if (valIMSI != null) {
            createNumber(valIMSI); // imsi
            tlv.valTyp = tlvIMSI;
            createTlv(pck);
        }
        if (valReordReq >= 0) {
            tlv.valDat[0] = (byte) valReordReq; // recovery counter
            tlv.valTyp = tlvReordReq;
            createTlv(pck);
        }
        if (valRecovery >= 0) {
            tlv.valDat[0] = (byte) valRecovery; // recovery counter
            tlv.valTyp = tlvRecovery;
            createTlv(pck);
        }
        if (valSelectMode >= 0) {
            tlv.valDat[0] = (byte) valSelectMode; // selection mode
            tlv.valTyp = tlvSelectMode;
            createTlv(pck);
        }
        if (valTeid1 != 0) {
            bits.msbPutD(tlv.valDat, 0, valTeid1); // tunnel endpoint id for data
            tlv.valTyp = tlvTeid1;
            createTlv(pck);
        }
        if (valTeidCp != 0) {
            bits.msbPutD(tlv.valDat, 0, valTeidCp); // tunnel endpoint id for control
            tlv.valTyp = tlvTeidCp;
            createTlv(pck);
        }
        if (valTeardown >= 0) {
            tlv.valDat[0] = (byte) valTeardown; // nsapi
            tlv.valTyp = tlvTeardown;
            createTlv(pck);
        }
        if (valNSAPI >= 0) {
            tlv.valDat[0] = (byte) valNSAPI; // nsapi
            tlv.valTyp = tlvNSAPI;
            createTlv(pck);
        }
        if (valChargChar >= 0) {
            bits.msbPutW(tlv.valDat, 0, valChargChar); // normal charging
            tlv.valTyp = tlvChargChar;
            createTlv(pck);
        }
        if (valChargID != 0) {
            bits.msbPutD(tlv.valDat, 0, valChargID); // charging id
            tlv.valTyp = tlvChargID;
            createTlv(pck);
        }
        if (valEndUserAddr >= 0) {
            bits.msbPutW(tlv.valDat, 0, valEndUserAddr); // mode
            tlv.valTyp = tlvEndUserAddr;
            tlv.valSiz = 2;
            switch (valEndUserAddr) {
                case adrPpp:
                    break;
                case adrIp4:
                    valEndUserAddr4.toBuffer(tlv.valDat, tlv.valSiz); // address
                    tlv.valSiz += valEndUserAddr4.getSize();
                    break;
                case adrIp6:
                    valEndUserAddr6.toBuffer(tlv.valDat, tlv.valSiz); // address
                    tlv.valSiz += valEndUserAddr6.getSize();
                    break;
                case adrIp46:
                    valEndUserAddr4.toBuffer(tlv.valDat, tlv.valSiz); // address
                    tlv.valSiz += valEndUserAddr4.getSize();
                    valEndUserAddr6.toBuffer(tlv.valDat, tlv.valSiz); // address
                    tlv.valSiz += valEndUserAddr6.getSize();
                    break;
            }
            createTlv(pck);
        }
        if (valAccessPointName != null) {
            tlv.valDat[0] = (byte) valAccessPointName.length(); // length of apn
            bits.byteCopy(valAccessPointName.getBytes(), 0, tlv.valDat, 1, valAccessPointName.length()); // apn
            tlv.valTyp = tlvAccessPointName;
            tlv.valSiz = valAccessPointName.length() + 1;
            createTlv(pck);
        }
        if (valGSNaddr != null) {
            if (valGSNaddr.isIPv4()) {
                addrIPv4 a = valGSNaddr.toIPv4();
                a.toBuffer(tlv.valDat, 0); // ggsn address
                tlv.valSiz = addrIPv4.size;
            } else {
                addrIPv6 a = valGSNaddr.toIPv6();
                a.toBuffer(tlv.valDat, 0); // ggsn address
                tlv.valSiz = addrIPv6.size;
            }
            tlv.valTyp = tlvGSNaddr;
            createTlv(pck);
        }
        if (valMSISDN != null) {
            tlv.valSiz = createNumber(valMSISDN); // msisdn
            tlv.valTyp = tlvMSISDN;
            createTlv(pck);
        }
        if (valQOSpro != 0) {
            bits.msbPutD(tlv.valDat, 0, valQOSpro); // best effort
            tlv.valSiz = 4;
            tlv.valTyp = tlvQOSpro;
            createTlv(pck);
        }
        if (valIMEI != null) {
            tlv.valSiz = createNumber(valIMEI); // imei
            tlv.valTyp = tlvIMEI;
            createTlv(pck);
        }
        flags = flgSeq;
        npduNum = 0;
        extTyp = 0;
        createHeader(pck);
        return pck;
    }

}
