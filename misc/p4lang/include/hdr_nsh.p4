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

#ifndef _NSH_P4_
#define _NSH_P4_

header nsh_t {
    bit<2>    version;
    bit<1>    oam;
    bit<1>    res1;
    bit<6>    ttl;
    bit<6>    length;
    bit<4>    res2;
    bit<4>    md_type;
    bit<8>    next_proto;
    bit<24>   sp;
    bit<8>    si;
}

#endif // _NSH_P4_
