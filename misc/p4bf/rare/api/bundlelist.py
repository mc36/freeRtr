from ..bf_gbl_env.cst_env import *


def setBundleAdmStatus(self, op_type, bundle_id, member_id_list):
    tbl_global_path = "ig_ctl.ig_ctl_bundle"
    tbl_apr_name = "%s.apr_bundle" % (tbl_global_path)
    tbl_ase_name = "%s.ase_bundle" % (tbl_global_path)
    tbl_nexthop_bundle_name = "%s.tbl_nexthop_bundle" % (tbl_global_path)
    max_grp_size = 120
    member_id_list.pop()
    member_id_list = list(map(int, member_id_list))
    member_status_list = []
    # actionprofile p4 objects are actually an object table
    tbl_apr_bundle = self.bfgc.bfrt_info.table_get(tbl_apr_name)
    # actionselector p4 objects are actually an object table
    tbl_ase_bundle = self.bfgc.bfrt_info.table_get(tbl_ase_name)
    # tbl_nexthop_bundle
    tbl_nexthop_bundle = self.bfgc.bfrt_info.table_get(tbl_nexthop_bundle_name)
    member_status_list = []
    for member_id in member_id_list:
        member_status_list.append(True)

    if op_type == WRITE:
        for member_id in member_id_list:
            tbl_apr_bundle_key = [
                tbl_apr_bundle.make_key(
                    [gc.KeyTuple("$ACTION_MEMBER_ID", member_id)]
                )
            ]
            tbl_apr_bundle_data = [
                tbl_apr_bundle.make_data(
                    [gc.DataTuple("port", member_id)],
                    "ig_ctl.ig_ctl_bundle.act_send_to_member",
                )
            ]

            try:
               tbl_apr_bundle.entry_add(
                   self.bfgc.target, tbl_apr_bundle_key, tbl_apr_bundle_data
               )
            except gc.BfruntimeRpcException as e:
                logger.warning("bundle_add - %s" %  e.__str__())
                logger.warning("bundle_add - GRPC error code: %s" % e.grpc_error.code())


        tbl_ase_bundle_key = [
            tbl_ase_bundle.make_key([gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)])
        ]

        tbl_ase_bundle_data = [
            tbl_ase_bundle.make_data(
                [
                    gc.DataTuple("$MAX_GROUP_SIZE", max_grp_size),
                    gc.DataTuple("$ACTION_MEMBER_ID", int_arr_val=member_id_list),
                    gc.DataTuple(
                        "$ACTION_MEMBER_STATUS", bool_arr_val=member_status_list
                    ),
                ]
            )
        ]

        tbl_ase_bundle.entry_add(
            self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
        )

        tbl_nexthop_bundle_key = [
            tbl_nexthop_bundle.make_key([gc.KeyTuple("ig_md.output_id", bundle_id)])
        ]

        tbl_nexthop_bundle_data = [
            tbl_nexthop_bundle.make_data(
                [gc.DataTuple("$SELECTOR_GROUP_ID", bundle_id)]
            )
        ]

        tbl_nexthop_bundle.entry_add(
            self.bfgc.target, tbl_nexthop_bundle_key, tbl_nexthop_bundle_data
        )

    elif op_type == UPDATE:

        bundle_id_entry = tbl_ase_bundle.entry_get(
            self.bfgc.target,
            [
                tbl_ase_bundle.make_key(
                    [gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)]
                )
            ],
            {"from_hw": False},
        )

        data_dict = next(bundle_id_entry)[0].to_dict()

        mem_dict_recv = {
            data_dict["$ACTION_MEMBER_ID"][i]: data_dict["$ACTION_MEMBER_STATUS"][i]
            for i in range(0, len(data_dict["$ACTION_MEMBER_ID"]))
        }

        member_id_to_add = []
        member_id_to_del = []

        for member_id in member_id_list:
            if not (member_id in mem_dict_recv.keys()):
                member_id_to_add.append(member_id)

        for member_id in mem_dict_recv.keys():
            if not (member_id in member_id_list):
                member_id_to_del.append(member_id)

        if len(member_id_to_add) != 0:
            for member_id in member_id_to_add:
                tbl_apr_bundle_key = [
                    tbl_apr_bundle.make_key(
                        [gc.KeyTuple("$ACTION_MEMBER_ID", member_id)]
                    )
                ]
                tbl_apr_bundle_data = [
                    tbl_apr_bundle.make_data(
                        [gc.DataTuple("port", member_id)],
                        "ig_ctl.ig_ctl_bundle.act_send_to_member",
                    )
                ]

                tbl_apr_bundle.entry_add(
                    self.bfgc.target, tbl_apr_bundle_key, tbl_apr_bundle_data
                )

            tbl_ase_bundle_key = [
                tbl_ase_bundle.make_key(
                    [gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)]
                )
            ]

            tbl_ase_bundle_data = [
                tbl_ase_bundle.make_data(
                    [
                        gc.DataTuple("$MAX_GROUP_SIZE", max_grp_size),
                        gc.DataTuple(
                            "$ACTION_MEMBER_ID", int_arr_val=member_id_list
                        ),
                        gc.DataTuple(
                            "$ACTION_MEMBER_STATUS", bool_arr_val=member_status_list
                        ),
                    ]
                )
            ]

            tbl_ase_bundle.entry_mod(
                self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
            )

        if len(member_id_to_del) != 0:
            tbl_ase_bundle_key = [
                tbl_ase_bundle.make_key(
                    [gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)]
                )
            ]

            tbl_ase_bundle_data = [
                tbl_ase_bundle.make_data(
                    [
                        gc.DataTuple("$MAX_GROUP_SIZE", max_grp_size),
                        gc.DataTuple(
                            "$ACTION_MEMBER_ID", int_arr_val=member_id_list
                        ),
                        gc.DataTuple(
                            "$ACTION_MEMBER_STATUS", bool_arr_val=member_status_list
                        ),
                    ]
                )
            ]

            tbl_ase_bundle.entry_mod(
                self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
            )

            for member_id in member_id_to_del:
                tbl_apr_bundle_key = [
                    tbl_apr_bundle.make_key(
                        [gc.KeyTuple("$ACTION_MEMBER_ID", member_id)]
                    )
                ]
                tbl_apr_bundle.entry_del(self.bfgc.target, tbl_apr_bundle_key)

    elif op_type == DELETE:

        bundle_id_entry = tbl_ase_bundle.entry_get(
            self.bfgc.target,
            [
                tbl_ase_bundle.make_key(
                    [gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)]
                )
            ],
            {"from_hw": False},
        )

        data_dict = next(bundle_id_entry)[0].to_dict()

        mem_dict_recv = {
            data_dict["$ACTION_MEMBER_ID"][i]: data_dict["$ACTION_MEMBER_STATUS"][i]
            for i in range(0, len(data_dict["$ACTION_MEMBER_ID"]))
        }

        tbl_nexthop_bundle_key = [
            tbl_nexthop_bundle.make_key([gc.KeyTuple("ig_md.output_id", bundle_id)])
        ]

        tbl_nexthop_bundle.entry_del(self.bfgc.target, tbl_nexthop_bundle_key)

        tbl_ase_bundle_key = [
            tbl_ase_bundle.make_key([gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)])
        ]
        tbl_ase_bundle.entry_del(self.bfgc.target, tbl_ase_bundle_key)

        for member_id in mem_dict_recv.keys():
            tbl_apr_bundle_key = [
                tbl_apr_bundle.make_key(
                    [gc.KeyTuple("$ACTION_MEMBER_ID", member_id)]
                )
            ]
            tbl_apr_bundle.entry_del(self.bfgc.target, tbl_apr_bundle_key)

        if member_id_list.sort() != mem_dict_recv.keys().sort():
            logger.warning(
                "[setBundleAdmStatus]: "
                "Member list from Control Plane is different "
                "from entry list in ActionSelector table"
            )
