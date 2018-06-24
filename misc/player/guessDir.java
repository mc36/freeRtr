
/**
 * guess directory
 *
 * @author matecsaba
 */
public class guessDir {

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
        findSongs fs = new findSongs();
        fs.doFind(s);
        fs.doSort();
        fs.doDir();
        playerUtil.put(fs.art + " - " + fs.dat + " - " + fs.alb);
    }

}
