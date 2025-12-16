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

#ifndef _BUNDLE_P4_
#define _BUNDLE_P4_

#define IPV4_IPV6_HASH   1

/*
   As per INTEL/BAREFOOT recommendation
   should be enable by default
   except for round robin hash algorithm selection
*/
#ifdef NO_SCRAMBLE
#define HAVE_SCRAMBLE 0
#else
#define HAVE_SCRAMBLE 1
#endif

#ifndef RESILIENT_SELECTION
#define RESILIENT_SELECTION 0
#endif

#ifndef MAX_PROFILE_MEMBERS
#define MAX_PROFILE_MEMBERS 2048
#endif

#ifndef MAX_GROUP_SIZE
#define MAX_GROUP_SIZE 120
#endif

#ifndef MAX_GROUPS
#define MAX_GROUPS 1024
#endif

/*
 * INTEL/BAREFOOT guidelines
 * check BA-102 notes & training lab
 */
#if RESILIENT_SELECTION == 0
const SelectorMode_t SELECTION_MODE = SelectorMode_t.FAIR;
#define BASE_HASH_WIDTH 14
#else
const SelectorMode_t SELECTION_MODE = SelectorMode_t.RESILIENT;
#define BASE_HASH_WIDTH 51
#endif /* RESILIENT_SELECTION */

#if MAX_GROUP_SIZE <= 120
#define SUBGROUP_BITS 0
#elif MAX_GROUP_SIZE <= 3840
#define SUBGROUP_BITS 10
#elif MAX_GROUP_SIZE <= 119040
#define SUBGROUP_BITS 15
#else
#error "Maximum Group Size cannot exceed 119040 members on Tofino"
#endif /* MAX_GROUP_SIZE */

/*
 * HASH_WIDTH final definition
 */
#define HASH_WIDTH (BASE_HASH_WIDTH + SUBGROUP_BITS)

const bit<32> BUNDLE_SIZE = 16384;


/*
 * Since we will be calculating hash in 32-bit pieces, we will have this
 * definition, which will be either bit<32>, bit<64> or bit<96> depending
 * on HASH_WIDTH
 */
typedef bit<(((HASH_WIDTH + 32)/32)*32)> selector_hash_t;

#endif // _BUNDLE_P4_
