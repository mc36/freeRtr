
/**
 * announce stream
 *
 * @author matecsaba
 */
public class announce {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        byte[] res = packer.sdp_generate(args[0], args[1], args[2]);
        System.out.println("echo \"");
        System.out.println(new String(res));
        System.out.println("\" | ffplay -protocol_whitelist file,fd,udp,rtp -");
        packer rtp = packer.sender("224.2.127.254", "9875");
        for (;;) {
            rtp.sap_announce(res, res.length, args[1], args[2]);
            Thread.sleep(15000);
        }
    }

}
