
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
            System.out.println("usage: java this <group> <source> <port>");
            return;
        }
        byte[] res = packer.generateSdp(args[0], args[1], args[2]);
        System.out.println("echo \"");
        System.out.println(new String(res));
        System.out.println("\" | ffplay -protocol_whitelist file,fd,udp,rtp -");
        packer rtp = packer.sender("224.2.127.254", "9875");
        for (;;) {
            rtp.announceSap(res, res.length, args[1], args[2]);
            Thread.sleep(15000);
        }
    }

}
