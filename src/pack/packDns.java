package pack;

import java.util.ArrayList;
import java.util.List;

/**
 * domain name protocol (rfc1035) packet
 *
 * @author matecsaba
 */
public class packDns {

    /**
     * default port number
     */
    public final static int portNum = 53;

    /**
     * identifier
     */
    public int id;

    /**
     * result code
     */
    public int result;

    /**
     * operation code
     */
    public int opcode;

    /**
     * recursion available
     */
    public boolean recAvail;

    /**
     * recursion desired
     */
    public boolean recDsrd;

    /**
     * truncated
     */
    public boolean truncated;

    /**
     * authoritative answer
     */
    public boolean authoritative;

    /**
     * query/response
     */
    public boolean response;

    /**
     * queries
     */
    public List<packDnsRec> queries = new ArrayList<packDnsRec>();

    /**
     * answers
     */
    public List<packDnsRec> answers = new ArrayList<packDnsRec>();

    /**
     * name servers
     */
    public List<packDnsRec> servers = new ArrayList<packDnsRec>();

    /**
     * additional records
     */
    public List<packDnsRec> addition = new ArrayList<packDnsRec>();

    /**
     * standard query
     */
    public final static int opcodeQuery = 0;

    /**
     * inverse query
     */
    public final static int opcodeInquery = 1;

    /**
     * server status
     */
    public final static int opcodeStatus = 2;

    /**
     * success
     */
    public final static int resultSuccess = 0;

    /**
     * format error
     */
    public final static int resultFormat = 1;

    /**
     * server error
     */
    public final static int resultServer = 2;

    /**
     * name error
     */
    public final static int resultName = 3;

    /**
     * not implemented
     */
    public final static int resultSupport = 4;

    /**
     * not allowed
     */
    public final static int resultRefused = 5;

    /**
     * recursion available
     */
    public final int flagRecAvail = 0x0080;

    /**
     * recursion desired
     */
    public final int flagRecDsrd = 0x0100;

    /**
     * truncated packet
     */
    public final int flagTruncated = 0x0200;

    /**
     * authoritative answer
     */
    public final int flagAuthoritative = 0x0400;

    /**
     * response
     */
    public final int flagResponse = 0x8000;

    /**
     * convert opcode to string
     *
     * @param i opcode
     * @return string representing opcode
     */
    public static String opcode2str(int i) {
        switch (i) {
            case opcodeQuery:
                return "query";
            case opcodeInquery:
                return "inverse";
            case opcodeStatus:
                return "status";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert result to string
     *
     * @param i result
     * @return string representing result
     */
    public static String result2str(int i) {
        switch (i) {
            case resultSuccess:
                return "success";
            case resultFormat:
                return "formatError";
            case resultServer:
                return "serverError";
            case resultName:
                return "nameError";
            case resultSupport:
                return "notImplemented";
            case resultRefused:
                return "refused";
            default:
                return "unknown=" + i;
        }
    }

    private String listRRs(List<packDnsRec> lst) {
        String s = "";
        for (int i = 0; i < lst.size(); i++) {
            packDnsRec ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            s += "(" + ntry.toUserStr(" ") + ")";
        }
        return s;
    }

    public String toString() {
        return "op=" + opcode2str(opcode) + " res=" + result2str(result) + " id=" + id + " resp=" + response + " author=" + authoritative + " trunc=" + truncated + " recAvail=" + recAvail + " recDesired=" + recDsrd + " query=" + listRRs(queries) + " answer=" + listRRs(answers) + " server=" + listRRs(servers) + " addition=" + listRRs(addition);
    }

    private boolean readRRs(packHolder pck, List<packDnsRec> lst, int max, int beg, boolean question) {
        for (int i = 0; i < max; i++) {
            packDnsRec rr = new packDnsRec();
            if (rr.parseHeader(pck, beg, question)) {
                return true;
            }
            lst.add(rr);
        }
        return false;
    }

    private void writeRRs(packHolder pck, List<packDnsRec> lst, boolean question) {
        for (int i = 0; i < lst.size(); i++) {
            packDnsRec ntry = lst.get(i);
            ntry.createHeader(pck, question);
        }
    }

    /**
     * parse header
     *
     * @param pck packet to use
     * @return false if successful, true if error happened
     */
    public boolean parseHeader(packHolder pck) {
        int msgBeg = pck.dataSize();
        id = pck.msbGetW(0); // id
        int flag = pck.msbGetW(2); // flags
        recAvail = (flag & flagRecAvail) != 0;
        recDsrd = (flag & flagRecDsrd) != 0;
        truncated = (flag & flagTruncated) != 0;
        authoritative = (flag & flagAuthoritative) != 0;
        response = (flag & flagResponse) != 0;
        result = flag & 0xf;
        opcode = (flag >>> 11) & 0xf;
        int que = pck.msbGetW(4); // questions
        int ans = pck.msbGetW(6); // answers
        int aut = pck.msbGetW(8); // authoritative
        int add = pck.msbGetW(10); // additional
        pck.getSkip(12);
        readRRs(pck, queries, que, msgBeg, true);
        readRRs(pck, answers, ans, msgBeg, false);
        readRRs(pck, servers, aut, msgBeg, false);
        readRRs(pck, addition, add, msgBeg, false);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to use
     */
    public void createHeader(packHolder pck) {
        int flag = (result & 0xf) | ((opcode & 0xf) << 11);
        if (recAvail) {
            flag |= flagRecAvail;
        }
        if (recDsrd) {
            flag |= flagRecDsrd;
        }
        if (truncated) {
            flag |= flagTruncated;
        }
        if (authoritative) {
            flag |= flagAuthoritative;
        }
        if (response) {
            flag |= flagResponse;
        }
        pck.msbPutW(0, id);
        pck.msbPutW(2, flag);
        pck.msbPutW(4, queries.size());
        pck.msbPutW(6, answers.size());
        pck.msbPutW(8, servers.size());
        pck.msbPutW(10, addition.size());
        pck.putSkip(12);
        writeRRs(pck, queries, true);
        writeRRs(pck, answers, false);
        writeRRs(pck, servers, false);
        writeRRs(pck, addition, false);
        pck.merge2beg();
    }

}
