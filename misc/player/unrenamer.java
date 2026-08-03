
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

/**
 * unrename files
 *
 * @author matecsaba
 */
public class unrenamer {

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        try {
            BufferedReader br = new BufferedReader(new FileReader(args[0]));
            for (;;) {
                String s = br.readLine();
                if (s == null) {
                    break;
                }
                int i = s.indexOf(" --> ");
                if (i < 0) {
                    playerUtil.put("no guess to " + s);
                    continue;
                }
                String a = s.substring(i + 5, s.length());
                s = s.substring(0, i);
                playerUtil.put(a + " --> " + s);
                try {
                    new File(a).renameTo(new File(s));
                } catch (Exception e) {
                    playerUtil.put("error");
                }
            }
            playerUtil.put("done!");
        } catch (Exception e) {
            playerUtil.put("exception!");
        }
    }

}
