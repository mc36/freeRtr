
/**
 * packet io kinds
 *
 * @author matecsaba
 */
public abstract class packet {

    /**
     * packer
     */
    protected final packer pck;

    /**
     * copy of codec
     */
    public final codec coder;

    /**
     * create instance
     *
     * @param p packer
     */
    protected packet(packer p) {
        pck = p;
        coder = p.coder;
    }

    /**
     * read data
     *
     * @param buf msb bytes
     * @return bytes
     * @throws Exception on error
     */
    public abstract int readKind(byte[] buf) throws Exception;

    /**
     * write scream data
     *
     * @param buf msb bytes
     * @param len length
     * @throws Exception on error
     */
    public abstract void writeKind(byte[] buf, int len) throws Exception;

}
