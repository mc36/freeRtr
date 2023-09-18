
import java.io.File;

/**
 * rfc renamer
 *
 * @author matecsaba
 */
public class renamer {

    private static String repairPath(String a) {
        if (a.endsWith("/")) {
            return a;
        }
        return a + "/";
    }

    private final static int needed = 4;

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        File[] fls = new File[0];
        String src = repairPath(args[0]);
        String trg = repairPath(args[1]);
        try {
            fls = new File(src).listFiles();
        } catch (Exception e) {
        }
        for (int i = 0; i < fls.length; i++) {
            File cur = fls[i];
            String org = cur.getName();
            if (!org.toLowerCase().startsWith("rfc")) {
                continue;
            }
            int s = org.length();
            String nw = org.substring(3, s);
            int o = nw.indexOf(".");
            if (o < 0) {
                continue;
            }
            s = nw.length();
            String ext = nw.substring(o, s).toLowerCase();
            nw = nw.substring(0, o);
            for (int p = nw.length(); p < needed; p++) {
                nw = "0" + nw;
            }
            nw = "rfc" + nw + ext;
            if (nw.equals(org)) {
                continue;
            }
            System.out.print("renaming " + org + " to " + nw + "... ");
            try {
                cur.renameTo(new File(trg + nw));
                System.out.print("done");
            } catch (Exception e) {
                System.out.print("error");
            }
            System.out.println(".");
        }
    }

}
