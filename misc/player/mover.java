
import java.io.File;
import java.util.List;

/**
 * move to collection
 *
 * @author matecsaba
 */
public class mover {

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        String s = "./";
        String t = args[0];
        if (args.length > 1) {
            s = args[1];
        }
        List<File> fl = findSongs.doFindDir(s);
        for (int i = 0; i < fl.size(); i++) {
            File f = fl.get(i);
            String a = f.getName();
            if (a.startsWith(".")) {
                continue;
            }
            if (!f.isDirectory()) {
                continue;
            }
            doDir(s, t, a);
        }
    }

    private static void doDir(String s, String t, String n) {
        String a = n.substring(0, 1).toLowerCase();
        if (!a.matches("[a-z]")) {
            a = "0";
        }
        t += a + "/";
        if (!new File(t + n).exists()) {
            playerUtil.put("done " + s + n);
            new File(s + n).renameTo(new File(t + n));
            return;
        }
        long ct = countDir(t + n);
        long cs = countDir(s + n);
        if (ct >= cs) {
            playerUtil.put("skip " + s + n + " because " + ct + " > " + cs + " by " + (ct - cs));
            return;
        }
        playerUtil.put("replace " + s + n + " because " + ct + " < " + cs + " by " + (cs - ct));
        new File(t + n).renameTo(new File(s + n + ".old"));
        new File(s + n).renameTo(new File(t + n));
        new File(s + n + ".old").renameTo(new File(s + n));
    }

    private static long countDir(String s) {
        List<File> fl = findSongs.doFindDir(s);
        long res = 0;
        for (int i = 0; i < fl.size(); i++) {
            res += fl.get(i).length();
        }
        return res;
    }
}
