package net.freertr.sec;

import net.freertr.cfg.cfgAll;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;

/**
 * negotiate security if needed
 *
 * @author matecsaba
 */
public class secClient {

    private secClient() {
    }

    /**
     * start secure connection
     *
     * @param pipe pipeline to use
     * @param proto protocol to use
     * @param pubkey pubkey to use
     * @param user username to send
     * @param pass password to send
     * @return secure pipeline, null on error
     */
    public static pipeSide openSec(pipeSide pipe, int proto, byte[] pubkey, String user, String pass) {
        if (pipe == null) {
            return null;
        }
        proto &= servGeneric.protoSec;
        if (proto == 0) {
            return pipe;
        }
        int tim = pipe.getTime();
        String a = pipe.settingsGet(pipeSetting.origin, "?");
        switch (proto & servGeneric.protoSec) {
            case servGeneric.protoSsh:
                secSsh ssh = new secSsh(pipe, new pipeLine(65536, false));
                ssh.startClient(pubkey, user, pass);
                pipe = ssh.getPipe();
                break;
            case servGeneric.protoTls:
            case servGeneric.protoDtls:
                boolean dtls = proto == servGeneric.protoDtls;
                secTls tls = new secTls(pipe, new pipeLine(65536, dtls), dtls);
                tls.minVer = 0x300 + cfgAll.tlsVerMin;
                tls.maxVer = 0x300 + cfgAll.tlsVerMax;
                if (a.length() > 1) {
                    tls.serverName = a;
                }
                tls.startClient(pubkey);
                pipe = tls.getPipe();
                break;
            case servGeneric.protoTelnet:
                secTelnet telnet = new secTelnet(pipe, new pipeLine(65536, false));
                telnet.startClient();
                pipe = telnet.getPipe();
                break;
        }
        pipe.setTime(tim);
        if (pipe.wait4ready(tim)) {
            pipe.setClose();
            return null;
        }
        pipe.settingsPut(pipeSetting.origin, a);
        return pipe;
    }

}
