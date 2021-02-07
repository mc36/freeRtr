/*
 * Copyright 2019-present GT RARE project
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

#ifndef _BIER_P4_
#define _BIER_P4_

header bier_t {
    bit<8> idver;
    bit<4> bsl;
    bit<20> entropy;
    bit<2> oam;
    bit<2> reserved;
    bit<6> dscp;
    bit<6> proto;
    bit<16> bfir;
    bit<32> bs0;
    bit<32> bs1;
    bit<32> bs2;
    bit<32> bs3;
    bit<32> bs4;
    bit<32> bs5;
    bit<32> bs6;
    bit<32> bs7;
}

#endif // _BIER_P4_
