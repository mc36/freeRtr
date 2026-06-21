
public class sender {

    public static void main(String[] args) throws Exception {
        decoder dec = new decoder(args[0], args[1]);
        rtper rtp = new rtper(args[2], args[3]);
        for (;;) {
            byte[] buf = new byte[rtper.payload];
            int i = dec.read(buf);
            if (i < 0) {
                break;
            }
            rtp.write(buf, i);
        }
    }

}
