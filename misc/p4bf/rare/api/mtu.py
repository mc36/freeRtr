from ..bf_gbl_env.var_env import *

# rx: ['mtu', '5', '1500', '\n']
def setPortMTU(self, port_id, mtu_size):
    resp = self.bfgc.port_table.entry_get(
        self.bfgc.target, [], {"from_hw": False}, p4_name=self.bfgc.p4_name
    )
    ACTIVE_PORTS = {}
    for d, k in resp:
        key_fields = k.to_dict()
        data_fields = d.to_dict()
        ACTIVE_PORTS[key_fields["$DEV_PORT"]["value"]] = data_fields["$PORT_NAME"]

    if port_id in ACTIVE_PORTS.keys():
        key = [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])]
        data = [
            self.bfgc.port_table.make_data(
                [
                    gc.DataTuple("$TX_MTU", mtu_size + 18),
                    gc.DataTuple("$RX_MTU", mtu_size + 18),
                ]
            )
        ]

        self.bfgc.port_table.entry_mod(self.bfgc.target, key, data)
        logger.debug("Port[%s] MTU set to %s" % (port_id, mtu_size))
