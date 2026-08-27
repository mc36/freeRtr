
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
        System.err.println("#define padln 64");
        System.err.println("#define srate " + consts.rate);
        System.err.println("#define smpbt " + consts.smpb);
        System.err.println("#define pktln " + consts.payl);
        System.err.println("#define wavln " + consts.wavl);
        System.err.println("#define rtpln " + consts.rtpl);
        System.err.println("#define rtpty " + consts.rtpt);
        System.err.println("#define scrbr " + consts.scrb);
        System.err.println("#define scrln " + consts.scrl);
        System.err.println("#define scrtp " + consts.scrt);
        System.err.println("#define vbabr " + consts.vbab());
        System.err.println("#define vbaln " + consts.vbal);
        System.err.println("#define vbamg " + consts.vbam);
        System.err.println("#define wfaln " + consts.wfal);
        System.err.println("#define wfamg " + consts.wfam);
        for (int i = 100; i < 2000; i++) {
            double a = i;
            double b = consts.rate * 2.0 * consts.smpb / a;
            double c = a / (2 * consts.smpb);
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
