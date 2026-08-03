from ..bf_gbl_env.var_env import *


def writePolkaPolyRules(self, op_type, poly):
    if self.polka == False:
        return
    try:
        tbl = self.bfgc.bfrt_info.table_get("ig_ctl.ig_ctl_polka.hash.algorithm")
        data_field_list = [
            gc.DataTuple("polynomial", (poly & 0xffff)),
        ]
        data_list = tbl.make_data(data_field_list, "user_defined")
        tbl.default_entry_set(self.bfgc.target, data_list)
    except KeyError as e:
        print("Error preparing entry from control plane: {}".format(e))
    except gc.BfruntimeRpcException as e:
        print("Error processing entry from control plane: {}".format(e))

    except grpc.RpcError as e:
        print(
            "Grpc channel error "
            "while processing entry from control plane: {}".format(e)
        )
