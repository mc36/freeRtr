package addr;

/**
 * empty address
 *
 * @author matecsaba
 */
public class addrEmpty extends addrType {

    /**
     * size of address
     */
    public final static int size = 0;

    public String toString() {
        return "none";
    }

    public int getSize() {
        return 0;
    }

    public addrEmpty copyBytes() {
        return new addrEmpty();
    }

    public boolean fromString(String s) {
        return false;
    }

}
