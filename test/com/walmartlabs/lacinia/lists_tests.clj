; Copyright (c) 2018-present Walmart, Inc.
;
; Licensed under the Apache License, Version 2.0 (the "License")
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

(ns com.walmartlabs.lacinia.lists-tests
  "Tests related to list types."
  (:require
    [clojure.test :refer [deftest is]]
    [com.walmartlabs.test-utils :refer [compile-schema execute]]))

(deftest nil-lists-stay-nil
  (let [schema (compile-schema "lists-schema.edn"
                               {:query/container
                                (constantly {:empty_list []
                                             :non_empty_list ["Peekaboo"]
                                             :null_list nil})})]
    (is (= {:data {:container {:empty_list []
                               :non_empty_list ["Peekaboo"]
                               :null_list nil}}}
           (execute schema
                    "{ container { empty_list non_empty_list null_list }}")))))

(deftest non-null-list-field-check
  (let [schema (compile-schema "lists-schema.edn"
                               {:query/container
                                (constantly {:empty_list []
                                             :non_empty_list ["Peekaboo"]
                                             :null_list nil})})]
    (is (= {:data {:container nil}
            :errors [{:locations [{:column 15
                                   :line 1}]
                      :message "Non-nullable field was null."
                      :path [:container
                             :non_null_list]}]}
           (execute schema
                    "{ container { non_null_list }}")))))


(deftest promotion-of-nils-to-empty-lists
  (let [schema (compile-schema "lists-schema.edn"
                               {:query/container
                                (constantly {:empty_list []
                                             :non_empty_list ["Peekaboo"]
                                             :null_list nil})}
                               {:promote-nils-to-empty-list? true})]
    (is (= {:data {:container {:empty_list []
                               :non_empty_list ["Peekaboo"]
                               :null_list []}}}
           (execute schema
                    "{ container { empty_list non_empty_list null_list }}")))))

(deftest nil-collapase
  (let [schema (compile-schema "nullability.edn"
                               {:query/user (constantly {:id "1"
                                                         :employer {:id "10"}})})]

    ;; Because Employer/name was nil, the employer field collapses to nil (because
    ;; User/employer is nullable).
    (is (= {:data {:user {:employer nil
                          :id "1"}}
            :errors [{:locations [{:column 30
                                   :line 4}]
                      :message "Non-nullable field was null."
                      :path [:user
                             :employer
                             :name]}]}
           (execute schema "
           { user(id: \"1\") {
               id
               employer { id name }
             }
           }")))))
