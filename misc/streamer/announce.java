
/**
 * announce stream
 *
 * @author matecsaba
 */
public class announce {

    public static void main(String[] args) throws Exception {
        byte[] res = rtper.genSdp(args[0], args[1], args[2]);
        System.out.println("echo \"");
        System.out.println(new String(res));
        System.out.println("\" | ffplay -protocol_whitelist file,fd,udp,rtp -");
        rtper rtp = new rtper("224.2.127.254", "9875");
        for (;;) {
            rtp.announce(res, res.length, args[1], args[2]);
            Thread.sleep(15000);
        }
    }

}
