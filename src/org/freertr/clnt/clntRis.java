package org.freertr.clnt;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgInit;
import org.freertr.enc.encJson;
import org.freertr.enc.encUrl;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;

/**
 * ris live client
 *
 * @author mate csaba
 */
public class clntRis {

    private final pipeSide pipe;

    private final addrIP peer;

    /**
     * create instance
     *
     * @param p pipeline to use
     * @param a address of peer
     */
    public clntRis(pipeSide p, addrIP a) {
        pipe = p;
        peer = a.copyBytes();
    }

    /**
     * perform client connection
     *
     * @param src url to use
     */
    public void clntConnect(encUrl src) {
        src.addParam("format", "sse");
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.linePut("GET " + src.toURL(false, false, true, true) + " HTTP/1.1");
        pipe.linePut("User-Agent: " + cfgInit.versionAgent);
        pipe.linePut("Host: " + src.server);
        pipe.linePut("");
    }

    /**
     * perform server connection
     */
    public void servConnect() {
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.linePut("HTTP/1.1 200 OK");
        pipe.linePut("Server: " + cfgInit.versionAgent);
        pipe.linePut("Content-Type: text/event-stream");
        pipe.linePut("Connection: keep-alive");
        pipe.linePut("Cache-Control: no-cache");
        pipe.linePut("");
    }

    /**
     * read up one packet
     *
     * @param pck packet to read
     * @return 0 on ok, 1 on error, 2=skip
     */
    public int readPacket(packHolder pck) {
        pck.clear();
        if (pipe.isClosed() != 0) {
            return 1;
        }
        String a = pipe.lineGet(0x11);
        if (!a.startsWith("data:")) {
            return 2;
        }
        String s = encJson.getValue(a, "peer");
        if (s == null) {
            return 2;
        }
        if (pck.IPsrc.fromString(s)) {
            return 2;
        }
        pck.IPtrg.setAddr(peer);
        s = encJson.getValue(a, "peer_asn");
        if (s == null) {
            return 2;
        }
        pck.INTiface = bits.str2num(s);
        s = encJson.getValue(a, "raw");
        if (s == null) {
            return 2;
        }
        int o = s.length() / 2;
        for (int i = 0; i < o; i++) {
            pck.putByte(0, bits.fromHex(s.substring(i * 2, i * 2 + 2)));
            pck.putSkip(1);
        }
        pck.merge2end();
        pck.INTtime = bits.getTime();
        return 0;
    }

    /**
     * write one packet
     *
     * @param pck packet to send
     */
    public void writePacket(packHolder pck) {
        String a = "";
        byte[] b = pck.getCopy();
        for (int i = 0; i < b.length; i++) {
            a += bits.toHexB(b[i]);
        }
        a = "\r\n" + "event: ris_message\r\n" + "data: {\"timestamp\":" + (pck.INTtime / 1000) + ",\"peer\":\"" + pck.IPsrc + "\",\"peer_asn\":\"" + pck.INTiface + "\",\"host\":\"" + peer + "\",\"type\":\"UPDATE\",raw\":\"" + a + "\"}\r\n";
        b = a.getBytes();
        pipe.nonBlockPut(b, 0, b.length);
    }

}
