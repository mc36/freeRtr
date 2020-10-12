package clnt;

import serv.servStreamingMdt;

/**
 * telemetry sender
 *
 * @author matecsaba
 */
public class clntTelemetry {

    /**
     * target
     */
    public String target;

    /**
     * port
     */
    public int port = servStreamingMdt.port;

    /**
     * proxy
     */
    public clntProxy proxy;

    /**
     * running
     */
    public boolean need2run;

    /**
     * stop working
     */
    public void stopWork() {
        if (!need2run) {
            return;
        }
        need2run = false;
    }

    /**
     * stop working
     */
    public void startWork() {
        if (need2run) {
            return;
        }
        need2run = true;
    }

}
