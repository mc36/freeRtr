from ..bf_gbl_env.var_env import *

ACTIVE_PORTS = {}

# rx: ['mtu', '5', '1500', '\n']
def setPortMTU(self, port_id, mtu_size):
    if port_id in ACTIVE_PORTS.keys():
        key=[self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])]
        data=[self.bfgc.port_table.make_data(
                  [
                      gc.DataTuple("$TX_MTU", mtu_size+18),
                      gc.DataTuple("$RX_MTU", mtu_size+18),
                  ]
              )]

        self.bfgc.port_table.entry_mod(self.bfgc.target,key,data)
        logger.debug("Port[%s] MTU set to %s" % (port_id,mtu_size))
