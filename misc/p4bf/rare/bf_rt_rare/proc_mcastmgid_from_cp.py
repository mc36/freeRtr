from ..bf_gbl_env.cst_env import *

def _processMcastMgidFromControlPlane(
    self,
    op_type,
    mgid,
    nodes,
    exclud,
):
    try:
        tbl_name = "$pre.mgid"
        tbl = self.bfgc.bfrt_info.table_get(tbl_name)
        key_field_list = [
            gc.KeyTuple('$MGID', mgid)
        ]
        data_field_list = [
            gc.DataTuple("$MULTICAST_NODE_ID", int_arr_val=nodes),
            gc.DataTuple("$MULTICAST_NODE_L1_XID_VALID", bool_arr_val=exclud),
            gc.DataTuple("$MULTICAST_NODE_L1_XID", int_arr_val=exclud),
        ]
        key_list = [tbl.make_key(key_field_list)]
        data_list = [tbl.make_data(data_field_list)]
    except KeyError as e:
        print("Error preparing entry to control plane: {}".format(e))
        return

    try:
        if op_type == WRITE:
            tbl.entry_add(self.bfgc.target, key_list, data_list)
            #### tbl.entry_mod_inc(self.bfgc.target, key_list, data_list, bfrt_pb2.TableModIncFlag.MOD_INC_ADD)
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
