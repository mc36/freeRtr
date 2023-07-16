
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * non ascii
 *
 * @author matecsaba
 */
public class nonAscii {

    /**
     * check if a string is usascii7bit or not
     *
     * @param a string to check
     * @return false if safe
     */
    protected static final boolean doNonAscii(String a) {
        if (a == null) {
            return true;
        }
        byte[] b = a.getBytes();
        for (int i = 0; i < b.length; i++) {
            int c = b[i];
            if (c < 0x20) {
                return true;
            }
            if (c >= 0x7e) {
                return true;
            }
        }
        return false;
    }

    /**
     * add non ascii names
     *
     * @param fl list of files
     * @param na not ascii
     * @param oa only ascii
     */
    protected static final void doNascii(List<File> fl, List<File> na, List<File> oa) {
        for (int i = 0; i < fl.size(); i++) {
            File fi = fl.get(i);
            String a = fi.getAbsolutePath();
            if (doNonAscii(a)) {
                na.add(fi);
            } else {
                oa.add(fi);
            }
        }
    }

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        String s = "./";
        if (args.length > 0) {
            s = args[0];
        }
        playerUtil.put("scanning " + s);
        List<File> na = new ArrayList<File>();
        List<File> oa = new ArrayList<File>();
        List<File> fl = findSongs.doFindDir(s);
        doNascii(fl, na, oa);
        for (int i = 0; i < na.size(); i++) {
            File ntry = na.get(i);
            playerUtil.put("not ascii " + ntry.getAbsolutePath());
        }
        playerUtil.put("found " + oa.size() + " ok and " + na.size() + " not ok entries");//////////////
    }

}
