from ..bf_gbl_env.cst_env import *

def _processEntryFromControlPlane(
    self,
    op_type,
    tbl_name,
    key_field_list,
    data_field_list,
    tbl_action_name,
    key_annotation_fields,
    data_annotation_fields,
):
    try:
        tbl = self.bfgc.bfrt_info.table_get(tbl_name)

        for annotation in key_annotation_fields.keys():
            tbl.info.key_field_annotation_add(
                annotation, key_annotation_fields[annotation]
            )

        for annotation in data_annotation_fields.keys():
            tbl.info.data_field_annotation_add(
                annotation, tbl_action_name, data_annotation_fields[annotation]
            )

        key_list = [tbl.make_key(key_field_list)]
        data_list = [tbl.make_data(data_field_list, tbl_action_name)]
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
