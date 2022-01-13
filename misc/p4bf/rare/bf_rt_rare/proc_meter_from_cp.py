from ..bf_gbl_env.cst_env import *

def _processMeterFromControlPlane(
    self,
    op_type,
    tbl_name,
    index,
    bytes,
    interval,
):
    if interval == 0:
        kbps = 0
    else:
        kbps = int(bytes * 8 / interval)
    try:
        tbl = self.bfgc.bfrt_info.table_get(tbl_name)
        key_field_list = [
            gc.KeyTuple("$METER_INDEX", index),
        ]
        data_field_list = [
            gc.DataTuple("$METER_SPEC_CIR_KBPS", kbps),
            gc.DataTuple("$METER_SPEC_PIR_KBPS", kbps),
            gc.DataTuple("$METER_SPEC_CBS_KBITS", 1),
            gc.DataTuple("$METER_SPEC_PBS_KBITS", 1),
        ]
        key_list = [tbl.make_key(key_field_list)]
        data_list = [tbl.make_data(data_field_list)]
    except KeyError as e:
        print("Error preparing entry to control plane: {}".format(e))
        return

    try:

        tbl.entry_mod(self.bfgc.target, key_list, data_list)
        logger.debug(
            "Updating entry in table:%s keys:%s act_param:%s",
            tbl_name,
            key_list,
            data_list,
        )

    except gc.BfruntimeRpcException as e:
        print("Error processing entry from control plane: {}".format(e))

    except grpc.RpcError as e:
        print(
            "Grpc channel error "
            "while processing entry from control plane: {}".format(e)
        )
