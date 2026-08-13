
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
        if (args.length < 3) {
            System.out.println("usage: java this <group> <source> <port> [sap-group]");
            return;
        }
        String a = args[0];
        byte[] res = packer.generateSdp(a, args[1], args[2]);
        System.out.println("echo \"");
        System.out.println(new String(res));
        System.out.println("\" | ffplay -protocol_whitelist file,fd,udp,rtp -");
        if (args.length > 3) {
            a = args[3];
        }
        System.out.println("announcing to " + a + "...");
        packer rtp = packer.sender(a, "9875");
        for (;;) {
            rtp.announceSap(res, res.length, args[1], args[2]);
            System.out.print(".");
            Thread.sleep(15000);
        }
    }

}
