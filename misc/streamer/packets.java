
/**
 * packet size printer
 *
 * @author matecsaba
 */
public class packets {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        System.err.println("#define srate " + devicer.rate);
        System.err.println("#define smpbt " + devicer.smpb);
        System.err.println("#define pktln " + devicer.payl);
        System.err.println("#define padln " + devicer.rtpl);
        System.err.println("#define payty " + devicer.rtpt);
        for (int i = 100; i < 2000; i++) {
            double a = i;
            double b = devicer.rate * 2.0 * devicer.smpb / a;
            double c = a / (2 * devicer.smpb);
            if (b != (int) b) {
                continue;
            }
            if (c != (int) c) {
                continue;
            }
            System.out.println(i + " " + b + " " + c);
        }
    }

}
