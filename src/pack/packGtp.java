package pack;

import util.bits;
import util.typLenVal;
import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;

/**
 * gprs tunneling protocol (3gpp29060) packet
 *
 * @author matecsaba
 */
public class packGtp {

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
    public typLenVal tlv = new typLenVal(0, 8, 8, 16, 1, 0, 3, 1, 0, 512, true);

    /**
     * protocol type
     */
    public static final int flgProt = 0x10;

    /**
     * extension header
     */
    public static final int flgExt = 0x04;

    /**
     * sequence number
     */
    public static final int flgSeq = 0x02;

    /**
     * n-pdu number
     */
    public static final int flgNpdu = 0x01;

    /**
     * ipv4
     */
    public static final int adrIp4 = 0xf121;

    /**
     * ipv6
     */
    public static final int adrIp6 = 0xf157;

    /**
     * ipv4 + ipv6
     */
    public static final int adrIp46 = 0xf18d;

    /**
     * ppp
     */
    public static final int adrPpp = 0xf001;

    /**
     * Echo Request
     */
    public static final int typEchoReq = 1;

    /**
     * Echo Response
     */
    public static final int typEchoRep = 2;

    /**
     * Version Not Supported
     */
    public static final int typUnsupp = 3;

    /**
     * Node Alive Request
     */
    public static final int typAliveReq = 4;

    /**
     * Node Alive Response
     */
    public static final int typAliveRep = 5;

    /**
     * Redirection Request
     */
    public static final int typRedirReq = 6;

    /**
     * Redirection Response
     */
    public static final int typRedirRep = 7;

    /**
     * Create PDP Context Request
     */
    public static final int typCreateReq = 16;

    /**
     * Create PDP Context Response
     */
    public static final int typCreateRep = 17;

    /**
     * Update PDP Context Request
     */
    public static final int typUpdateReq = 18;

    /**
     * Update PDP Context Response
     */
    public static final int typUpdateRep = 19;

    /**
     * Delete PDP Context Request
     */
    public static final int typDeleteReq = 20;

    /**
     * Delete PDP Context Response
     */
    public static final int typDeleteRep = 21;

    /**
     * Initiate PDP Context Activation Request
     */
    public static final int typInitReq = 22;

    /**
     * Initiate PDP Context Activation Response
     */
    public static final int typInitRep = 23;

    /**
     * Error Indication
     */
    public static final int typError = 26;

    /**
     * PDU Notification Request
     */
    public static final int typNotifyReq = 27;

    /**
     * PDU Notification Response
     */
    public static final int typNotifyRep = 28;

    /**
     * PDU Notification Reject Request
     */
    public static final int typNotifRejReq = 29;

    /**
     * PDU Notification Reject Response
     */
    public static final int typNotifRejRep = 30;

    /**
     * Supported Extension Headers Notification
     */
    public static final int typSupported = 31;

    /**
     * Send Routeing Information for GPRS Request
     */
    public static final int typRoutReq = 32;

    /**
     * Send Routeing Information for GPRS Response
     */
    public static final int typRoutRep = 33;

    /**
     * Failure Report Request
     */
    public static final int typFailReq = 34;

    /**
     * Failure Report Response
     */
    public static final int typFailRep = 35;

    /**
     * Note MS GPRS Present Request
     */
    public static final int typPresentReq = 36;

    /**
     * Note MS GPRS Present Response
     */
    public static final int typPresentRep = 37;

    /**
     * Identification Request
     */
    public static final int typIdentReq = 48;

    /**
     * Identification Response
     */
    public static final int typIdentRep = 49;

    /**
     * SGSN Context Request
     */
    public static final int typSgsnReq = 50;

    /**
     * SGSN Context Response
     */
    public static final int typSgsnRep = 51;

    /**
     * SGSN Context Acknowledge
     */
    public static final int typSgsnAck = 52;

    /**
     * Forward Relocation Request
     */
    public static final int typForwardReq = 53;

    /**
     * Forward Relocation Response
     */
    public static final int typForwardRep = 54;

    /**
     * Forward Relocation Complete
     */
    public static final int typForwardCmp = 55;

    /**
     * Relocation Cancel Request
     */
    public static final int typReloCanReq = 56;

    /**
     * Relocation Cancel Response
     */
    public static final int typRelocCanRep = 57;

    /**
     * Forward SRNS Context
     */
    public static final int typForwardSrnsReq = 58;

    /**
     * Forward Relocation Complete Acknowledge
     */
    public static final int typForwardAck = 59;

    /**
     * Forward SRNS Context Acknowledge
     */
    public static final int typForwardSrnsAck = 60;

    /**
     * RAN Information Relay
     */
    public static final int typRanRelay = 70;

    /**
     * MBMS Notification Request
     */
    public static final int typMbmsNotifReq = 96;

    /**
     * MBMS Notification Response
     */
    public static final int typMbmsNotifRep = 97;

    /**
     * MBMS Notification Reject Request
     */
    public static final int typMbmsRejReq = 98;

    /**
     * MBMS Notification Reject Response
     */
    public static final int typMbmsRejRep = 99;

    /**
     * Create MBMS Context Request
     */
    public static final int typCreateMbmsReq = 100;

    /**
     * Create MBMS Context Response
     */
    public static final int typCreateNbmsRep = 101;

    /**
     * Update MBMS Context Request
     */
    public static final int typUpdateMbmsReq = 102;

    /**
     * Update MBMS Context Response
     */
    public static final int typUpdateMbmsRep = 103;

    /**
     * Delete MBMS Context Request
     */
    public static final int typDeleteMbmsReq = 104;

    /**
     * Delete MBMS Context Response
     */
    public static final int typDeleteMbmsRep = 105;

    /**
     * MBMS Registration Request
     */
    public static final int typMbmsRegReq = 112;

    /**
     * MBMS Registration Response
     */
    public static final int typMbmsRegRep = 113;

    /**
     * MBMS De-Registration Request
     */
    public static final int typMbmsDelReq = 114;

    /**
     * MBMS De-Registration Response
     */
    public static final int typMbmsDelRep = 115;

    /**
     * MBMS Session Start Request
     */
    public static final int typMbmsStartReq = 116;

    /**
     * MBMS Session Start Response
     */
    public static final int typMbmsStartRep = 117;

    /**
     * MBMS Session Stop Request
     */
    public static final int typMbmsStopReq = 118;

    /**
     * MBMS Session Stop Response
     */
    public static final int typMbmsStopRep = 119;

    /**
     * MBMS Session Update Request
     */
    public static final int typMbmsUpdateReq = 120;

    /**
     * MBMS Session Update Response
     */
    public static final int typMbmsUpdateRep = 121;

    /**
     * MS Info Change Notification Request
     */
    public static final int typMsiChangeReq = 128;

    /**
     * MS Info Change Notification Response
     */
    public static final int typMsiChangeRep = 129;

    /**
     * Data Record Transfer Request
     */
    public static final int typDataRecReq = 240;

    /**
     * Data Record Transfer Response
     */
    public static final int typDataRecRep = 241;

    /**
     * End Marker
     */
    public static final int typEndMark = 254;

    /**
     * G-PDU
     */
    public static final int typGPDU = 255;

    /**
     * Cause
     */
    public static final int tlvCause = 1;

    /**
     * International Mobile Subscriber Identity (IMSI)
     */
    public static final int tlvIMSI = 2;

    /**
     * Routeing Area Identity (RAI)
     */
    public static final int tlvRAI = 3;

    /**
     * Temporary Logical Link Identity (TLLI)
     */
    public static final int tlvTLLI = 4;

    /**
     * Packet TMSI (P-TMSI)
     */
    public static final int tlvPTMSI = 5;

    /**
     * Reordering Required
     */
    public static final int tlvReordReq = 8;

    /**
     * Authentication Triplet
     */
    public static final int tlvAuthTrip = 9;

    /**
     * MAP Cause
     */
    public static final int tlvMAPcause = 11;

    /**
     * P-TMSI Signature
     */
    public static final int tlvPTMSIsig = 12;

    /**
     * MS Validated
     */
    public static final int tlvMSvalid = 13;

    /**
     * Recovery
     */
    public static final int tlvRecovery = 14;

    /**
     * Selection Mode
     */
    public static final int tlvSelectMode = 15;

    /**
     * Tunnel Endpoint Identifier Data I
     */
    public static final int tlvTeid1 = 16;

    /**
     * Tunnel Endpoint Identifier Control Plane
     */
    public static final int tlvTeidCp = 17;

    /**
     * Tunnel Endpoint Identifier Data II
     */
    public static final int tlvTeid2 = 18;

    /**
     * Teardown Ind
     */
    public static final int tlvTeardown = 19;

    /**
     * NSAPI
     */
    public static final int tlvNSAPI = 20;

    /**
     * RANAP Cause
     */
    public static final int tlvRANAPcause = 21;

    /**
     * RAB Context
     */
    public static final int tlvRABcontext = 22;

    /**
     * Radio Priority SMS
     */
    public static final int tlvRadioSMS = 23;

    /**
     * Radio Priority
     */
    public static final int tlvRadioPri = 24;

    /**
     * Packet Flow Id
     */
    public static final int tlvPackFlowId = 25;

    /**
     * Charging Characteristics
     */
    public static final int tlvChargChar = 26;

    /**
     * Trace Reference
     */
    public static final int tlvTraceRef = 27;

    /**
     * Trace Type
     */
    public static final int tlvTraceTyp = 28;

    /**
     * MS Not Reachable Reason
     */
    public static final int tlvMSunReach = 29;

    /**
     * Charging ID
     */
    public static final int tlvChargID = 127;

    /**
     * End User Address
     */
    public static final int tlvEndUserAddr = 128;

    /**
     * MM Context
     */
    public static final int tlvMMcontext = 129;

    /**
     * PDP Context
     */
    public static final int tlvPDPcontext = 130;

    /**
     * Access Point Name
     */
    public static final int tlvAccessPointName = 131;

    /**
     * Protocol Configuration Options
     */
    public static final int tlvProtCfgOpt = 132;

    /**
     * GSN Address
     */
    public static final int tlvGSNaddr = 133;

    /**
     * MS International PSTN/ISDN Number (MSISDN)
     */
    public static final int tlvMSISDN = 134;

    /**
     * Quality of Service Profile
     */
    public static final int tlvQOSpro = 135;

    /**
     * Authentication Quintuplet
     */
    public static final int tlvAuthQuin = 136;

    /**
     * Traffic Flow Template
     */
    public static final int tlvTrafFlowTemp = 137;

    /**
     * Target Identification
     */
    public static final int tlvTargetIdent = 138;

    /**
     * UTRAN Transparent Container
     */
    public static final int tlvUTRANtrans = 139;

    /**
     * RAB Setup Information
     */
    public static final int tlvRABsetup = 140;

    /**
     * Extension Header Type List
     */
    public static final int tlvExtHead = 141;

    /**
     * Trigger Id
     */
    public static final int tlvTrigId = 142;

    /**
     * OMC Identity
     */
    public static final int tlvOMCid = 143;

    /**
     * RAN Transparent Container
     */
    public static final int tlvRANtrans = 144;

    /**
     * PDP Context Prioritization
     */
    public static final int tlvPDPcontPri = 145;

    /**
     * Additional RAB Setup Information
     */
    public static final int tlvAddRABsetup = 146;

    /**
     * SGSN Number
     */
    public static final int tlvSGSNnum = 147;

    /**
     * Common Flags
     */
    public static final int tlvCommonFlags = 148;

    /**
     * APN Restriction
     */
    public static final int tlvAPNrestrict = 149;

    /**
     * Radio Priority LCS
     */
    public static final int tlvRadioLCS = 150;

    /**
     * RAT Type
     */
    public static final int tlvRATtype = 151;

    /**
     * User Location Information
     */
    public static final int tlvUserLocInfo = 152;

    /**
     * MS Time Zone
     */
    public static final int tlvMStimZon = 153;

    /**
     * IMEI(SV)
     */
    public static final int tlvIMEI = 154;

    /**
     * CAMEL Charging Information Container
     */
    public static final int tlvCAMELchargInfo = 155;

    /**
     * MBMS UE Context
     */
    public static final int tlvMBMSue = 156;

    /**
     * Temporary Mobile Group Identity (TMGI)
     */
    public static final int tlvTMGI = 157;

    /**
     * RIM Routing Address
     */
    public static final int tlvRIMroutAddr = 158;

    /**
     * MBMS Protocol Configuration Options
     */
    public static final int tlvMBMSprotCfg = 159;

    /**
     * MBMS Service Area
     */
    public static final int tlvMBMSservArea = 160;

    /**
     * Source RNC PDCP context info
     */
    public static final int tlvSrcRNCpdcp = 161;

    /**
     * Additional Trace Info
     */
    public static final int tlvAddTraceInfo = 162;

    /**
     * Hop Counter
     */
    public static final int tlvHopCount = 163;

    /**
     * Selected PLMN ID
     */
    public static final int tlvSelectPLMN = 164;

    /**
     * MBMS Session Identifier
     */
    public static final int tlvMBMSsessId = 165;

    /**
     * MBMS 2G/3G Indicator
     */
    public static final int tlvMBMSind = 166;

    /**
     * Enhanced NSAPI
     */
    public static final int tlvEnhNSAPI = 167;

    /**
     * MBMS Session Duration
     */
    public static final int tlvMBMSsessDur = 168;

    /**
     * Additional MBMS Trace Info
     */
    public static final int tlvAddMBMStrace = 169;

    /**
     * MBMS Session Repetition Number
     */
    public static final int tlvMBMSsessRepet = 170;

    /**
     * MBMS Time To Data Transfer
     */
    public static final int tlvMBMStime2data = 171;

    /**
     * BSS Container
     */
    public static final int tlvBSScont = 173;

    /**
     * Cell Identification
     */
    public static final int tlvCellId = 174;

    /**
     * PDU Numbers
     */
    public static final int tlvPDUnums = 175;

    /**
     * BSSGP Cause
     */
    public static final int tlvBSSGPcause = 176;

    /**
     * Required MBMS bearer capabilities
     */
    public static final int tlvReqMBMSbearCap = 177;

    /**
     * RIM Routing Address Discriminator
     */
    public static final int tlvRIMroutAddrDisc = 178;

    /**
     * List of set-up PFCs
     */
    public static final int tlvPFClist = 179;

    /**
     * PS Handover XID Parameters
     */
    public static final int tlvPShandXID = 180;

    /**
     * MS Info Change Reporting Action
     */
    public static final int tlvMSIchgReport = 181;

    /**
     * Direct Tunnel Flags
     */
    public static final int tlvDirTun = 182;

    /**
     * Correlation-ID
     */
    public static final int tlvCorrID = 183;

    /**
     * Bearer Control Mode
     */
    public static final int tlvBearCtrl = 184;

    /**
     * MBMS Flow Identifier
     */
    public static final int tlvMBMSflowId = 185;

    /**
     * MBMS IP Multicast Distribution
     */
    public static final int tlvMBMSmulticast = 186;

    /**
     * MBMS Distribution Acknowledgement
     */
    public static final int tlvMBMSdistrib = 187;

    /**
     * Reliable INTER RAT HANDOVER INFO
     */
    public static final int tlvRelRAThand = 188;

    /**
     * RFSP Index
     */
    public static final int tlvRFSPindex = 189;

    /**
     * Fully Qualified Domain Name (FQDN)
     */
    public static final int tlvFQDN = 190;

    /**
     * Evolved Allocation/Retention Priority I
     */
    public static final int tlvAllocRetPri1 = 191;

    /**
     * Evolved Allocation/Retention Priority II
     */
    public static final int tlvAllocRetPri2 = 192;

    /**
     * Extended Common Flags
     */
    public static final int tlvExtCommFlg = 193;

    /**
     * User CSG Information (UCI)
     */
    public static final int tlvUserCSGinfo = 194;

    /**
     * CSG Information Reporting Action
     */
    public static final int tlvCSGinfoReport = 195;

    /**
     * CSG ID
     */
    public static final int tlvCsgID = 196;

    /**
     * CSG Membership Indication (CMI)
     */
    public static final int tlvCSGmemberInd = 197;

    /**
     * Aggregate Maximum Bit Rate (AMBR)
     */
    public static final int tlvAggrMaxBitRate = 198;

    /**
     * UE Network Capability
     */
    public static final int tlvUEnetCapa = 199;

    /**
     * UE-AMBR
     */
    public static final int tlvUEambr = 200;

    /**
     * APN-AMBR with NSAPI
     */
    public static final int tlvAPNambr = 201;

    /**
     * Charging Gateway Address
     */
    public static final int tlvChargGwAddr = 251;

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
     * dump this packet
     *
     * @return string
     */
    public String dump() {
        String s = "flag=" + flags + " type=" + type2string(msgTyp) + " tun=" + tunId + " seq=" + seqNum + " npdu=" + npduNum
                + " ext=" + extTyp;
        s += " cause=" + valCause + " imsi=" + valIMSI + " reorder=" + valReordReq + " recovery=" + valRecovery + " select="
                + valSelectMode;
        s += " tunD=" + valTeid1 + " tunC=" + valTeidCp + " teardown=" + valTeardown + " nsapi=" + valNSAPI + " chrgChr="
                + valChargChar;
        s += " chrgId=" + valChargID + " addr=" + valEndUserAddr + " apn=" + valAccessPointName + " gsn=" + valGSNaddr
                + " isdn=" + valMSISDN;
        s += " qos=" + valQOSpro + " imei=" + valIMEI;
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
                    valEndUserAddr = bits.msbGetW(tlv.valDat, 0);
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
            bits.msbPutW(tlv.valDat, 0, valEndUserAddr); // ppp mode
            tlv.valTyp = tlvEndUserAddr;
            tlv.valSiz = 2;
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
            tlv.valSiz = 8;
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
