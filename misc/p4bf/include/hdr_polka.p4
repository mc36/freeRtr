/*
 * Copyright 2021-present Universidade Federal do Espirito Santo (UFES) and
 *                        Instituto Federal do Espirito Santo (IFES)
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

#ifndef _POLKA_P4_
#define _POLKA_P4_

#ifdef HAVE_POLKA
header polka_t {
    bit<8>          version;
    bit<8>          ttl;
    ethertype_t     proto;
    polka_route_t   routeid;
}
#endif

#endif // _POLKA_P4_
