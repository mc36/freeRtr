package org.freertr.sec;

import org.freertr.cfg.cfgAll;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servGeneric;

/**
 * negotiate security if needed
 *
 * @author matecsaba
 */
public class secClient {

    private secClient() {
    }

    /**
     * start ssh session
     *
     * @param console console to use
     * @param stream connection to use
     * @param proto protocol to use
     * @param pubkey public key
     * @param user username
     * @param pass password
     * @return pipeline of this connection, null=error
     */
    public static pipeSide startSecurity(pipeSide console, pipeSide stream, int proto, byte[] pubkey, String user, String pass) {
        proto &= servGeneric.protoSec;
        if (stream == null) {
            return null;
        }
        if (proto == 0) {
            return stream;
        }
        if (proto == servGeneric.protoSsh) {
            if (user == null) {
                console.strPut("username: ");
                user = console.lineGet(50);
            }
            if (pass == null) {
                console.strPut("password: ");
                int red;
                if (console.settingsGet(pipeSetting.passStar, false)) {
                    red = 51;
                } else {
                    red = 49;
                }
                pass = console.lineGet(red);
            }
        }
        console.strPut("securing connection");
        stream = openSec(stream, proto, pubkey, user, pass);
        if (stream == null) {
            console.linePut(" failed!");
            return null;
        }
        console.linePut(" ok!");
        return stream;
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
