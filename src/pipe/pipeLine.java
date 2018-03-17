package pipe;

/**
 * pipeline handler
 *
 * @author matecsaba
 */
public class pipeLine {

    /**
     * don't try the operation again, it won't work
     */
    public final static int wontWork = -2;

    /**
     * try later the operation again, possibly it will work
     */
    public final static int tryLater = -1;

    private pipeSide clnt2serv;

    /**
     * server to client pipe side
     */
    protected final pipeSide serv2clnt;

    private int nextEntry;

    /**
     * create new pipeline
     *
     * @param bufSize size of one direction buffer
     * @param blockMode set true to keep block boundaries
     */
    public pipeLine(int bufSize, boolean blockMode) {
        int bufferSize = chkSiz(bufSize);
        clnt2serv = new pipeSide(bufferSize, blockMode);
        serv2clnt = new pipeSide(bufferSize, blockMode);
        clnt2serv.peerSideOfPipeLine = serv2clnt;
        serv2clnt.peerSideOfPipeLine = clnt2serv;
        clnt2serv.doInact(true);
        serv2clnt.doInact(true);
        nextEntry = 0;
    }

    /**
     * check if pipeline in block mode
     *
     * @return true means yes, false means no
     */
    public boolean isBlockMode() {
        return serv2clnt.isBlockMode();
    }

    /**
     * maximum number of bytes in buffer
     *
     * @return bytes in buffer
     */
    public int getBufSize() {
        return serv2clnt.getBufSize();
    }

    /**
     * create new pipeline with copied properties
     *
     * @param sample where from clone the new instance
     * @param blockMode set true to keep block boundaries
     * @return created pipeline
     */
    public static pipeLine doClone(pipeLine sample, boolean blockMode) {
        return new pipeLine(sample.serv2clnt.getBufSize(), blockMode);
    }

    private static int chkSiz(int i) {
        final int bufSizMax = 8 * 1024 * 1024;
        final int bufSizMin = 64;
        if (i > bufSizMax) {
            i = bufSizMax;
        }
        if (i < bufSizMin) {
            i = bufSizMin;
        }
        i &= 0x3ffffff0;
        return i;
    }

    /**
     * get next side of pipeline
     *
     * @return side of pipeline, null if no more
     */
    public pipeSide getSide() {
        nextEntry += 1;
        if (nextEntry == 1) {
            return serv2clnt;
        }
        if (nextEntry == 2) {
            return clnt2serv;
        }
        return null;
    }

    /**
     * close both sides
     */
    public void setClose() {
        clnt2serv.setClose();
        serv2clnt.setClose();
    }

}
