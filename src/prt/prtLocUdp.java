package prt;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import pipe.pipeSide;

/**
 * local udp server
 *
 * @author matecsaba
 */
public class prtLocUdp {

    /**
     * do one session
     *
     * @param pipe pipe to use
     * @param sock socket to use
     */
    public static void doSession(pipeSide pipe, DatagramSocket sock) {
        pipe.setReady();
        new prtLocUdpRx(sock, pipe);
        new prtLocUdpTx(sock, pipe);
    }

}

class prtLocUdpRx implements Runnable {

    public DatagramSocket sock;

    public pipeSide pipe;

    public prtLocUdpRx(DatagramSocket clnt, pipeSide conn) {
        sock = clnt;
        pipe = conn;
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                byte[] buf = new byte[2048];
                DatagramPacket d = new DatagramPacket(buf, buf.length);
                sock.receive(d);
                int i = d.getLength();
                if (i < 1) {
                    break;
                }
                pipe.blockingPut(buf, 0, i);
            }
        } catch (Exception e) {
        }
        try {
            pipe.setClose();
            sock.close();
        } catch (Exception e) {
        }
    }

}

class prtLocUdpTx implements Runnable {

    public DatagramSocket sock;

    public pipeSide pipe;

    public prtLocUdpTx(DatagramSocket clnt, pipeSide conn) {
        sock = clnt;
        pipe = conn;
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                byte[] buf = new byte[2048];
                int i = pipe.blockingGet(buf, 0, buf.length);
                if (i < 1) {
                    break;
                }
                DatagramPacket d = new DatagramPacket(buf, i);
                sock.send(d);
            }
        } catch (Exception e) {
        }
        try {
            pipe.setClose();
            sock.close();
        } catch (Exception e) {
        }
    }

}
