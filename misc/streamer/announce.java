
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
        String grp = args[0];
        String src = args[1];
        byte[] res = packer.generateSdp(grp, src, args[2]);
        System.out.println("echo \"");
        System.out.println(new String(res));
        System.out.println("\" | ffplay -protocol_whitelist file,fd,udp,rtp -");
        if (args.length > 3) {
            grp = args[3];
        }
        System.out.println("announcing to " + grp + "...");
        packer rtp = packer.sender(grp, src, "9875");
        for (;;) {
            rtp.announceSap(res, res.length, src, args[2]);
            System.out.print(".");
            Thread.sleep(15000);
        }
    }

}
