from ..bf_gbl_env.var_env import *

# rx: ['mtu', '5', '1500', '\n']
def setPortMTU(self, port_id, mtu_size):
    if port_id not in self.active_ports:
        self.controlPlaneMsg("Attempting to set mtu for inactive port [%s]" % port_id)
        return

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
    logger.warning("Port[%s] MTU set to %s" % (port_id, mtu_size))
