from ..bf_gbl_env.cst_env import *

def portStats(self, port_id):
    try:
        resp = self.bfgc.stat_table.entry_get(
            self.bfgc.target,
            [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
            {"from_hw": True},
            p4_name=self.bfgc.p4_name)
        stats = next(resp)[0].to_dict()
    except Exception as e:
        self.controlPlaneMsg(
            "%s:%s - Error getting port stats for port [%s]: %s"
            % (
                self.class_name,
                inspect.currentframe().f_code.co_name,
                port_id,
                e
            )
        )
    else:
        self.file_w.write("stats_beg %s\n" % port_id)
        for k, v in stats.items():
            if k[0] != "$":
                continue
            self.file_w.write("stats_txt %s %s %s\n" % (port_id, k[1:], v))
        self.file_w.write("stats_end %s\n" % port_id)
        self.file_w.flush()

