from ..bf_gbl_env.cst_env import *

def _processMcastNodeFromControlPlane(
    self,
    op_type,
    nodeid,
    rid,
    port,
):
    try:
        tbl_name = "$pre.node"
        tbl = self.bfgc.bfrt_info.table_get(tbl_name)
        key_field_list = [
            gc.KeyTuple('$MULTICAST_NODE_ID', nodeid)
        ]
        data_field_list = [
            gc.DataTuple("$MULTICAST_RID", rid),
            gc.DataTuple("$DEV_PORT", int_arr_val=[port]),
        ]
        key_list = [tbl.make_key(key_field_list)]
        data_list = [tbl.make_data(data_field_list)]
    except KeyError as e:
        print("Error preparing entry to control plane: {}".format(e))
        return

    try:
        if op_type == WRITE:
            tbl.entry_add(self.bfgc.target, key_list, data_list)
            logger.debug(
                "Writing entry in table:%s keys:%s act_param:%s",
                tbl_name,
                key_list,
                data_list,
            )
        elif op_type == UPDATE:
            tbl.entry_mod(self.bfgc.target, key_list, data_list)
            logger.debug(
                "Updating entry in table:%s keys:%s act_param:%s",
                tbl_name,
                key_list,
                data_list,
            )
        elif op_type == DELETE:
            tbl.entry_del(self.bfgc.target, key_list)
            logger.debug(
                "Deleting entry in table:%s keys:%s act_param:%s",
                tbl_name,
                key_list,
                data_list,
            )
        else:
            print("Error op_type unknown")

    except gc.BfruntimeRpcException as e:
        print("Error processing entry from control plane: {}".format(e))

    except grpc.RpcError as e:
        print(
            "Grpc channel error "
            "while processing entry from control plane: {}".format(e)
        )
