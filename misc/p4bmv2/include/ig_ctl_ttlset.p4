/*
 * Copyright 2019-present GEANT RARE project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed On an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef _IG_CTL_ttlset_P4_
#define _IG_CTL_ttlset_P4_

control IngressControlTtlSet(inout headers hdr,
                             inout ingress_metadata_t ig_md,
                             inout standard_metadata_t ig_intr_md) {



    action act_ipv4_ttl(bit<8> ttl) {
        hdr.ipv4.ttl = ttl;
    }

    action act_ipv6_ttl(bit<8> ttl) {
        hdr.ipv6.hop_limit = ttl;
    }


    table tbl_ipv4_in {
        key = {
ig_md.source_id:
            exact;
        }
        actions = {
            act_ipv4_ttl;
            @defaultonly NoAction;
        }
        size = MAX_PORT;
        const default_action = NoAction();
    }

    table tbl_ipv4_out {
        key = {
ig_md.aclport_id:
            exact;
        }
        actions = {
            act_ipv4_ttl;
            @defaultonly NoAction;
        }
        size = MAX_PORT;
        const default_action = NoAction();
    }

    table tbl_ipv6_in {
        key = {
ig_md.source_id:
            exact;
        }
        actions = {
            act_ipv6_ttl;
            @defaultonly NoAction;
        }
        size = MAX_PORT;
        const default_action = NoAction();
    }

    table tbl_ipv6_out {
        key = {
ig_md.aclport_id:
            exact;
        }
        actions = {
            act_ipv6_ttl;
            @defaultonly NoAction;
        }
        size = MAX_PORT;
        const default_action = NoAction();
    }



    apply {
        if (hdr.ipv4.isValid() && (ig_md.ipv4_valid==1))  {
            tbl_ipv4_in.apply();
            tbl_ipv4_out.apply();
        }
        if (hdr.ipv6.isValid() && (ig_md.ipv6_valid==1))  {
            tbl_ipv6_in.apply();
            tbl_ipv6_out.apply();
        }
    }
}


#endif // _IG_CTL_ttlset_P4_
