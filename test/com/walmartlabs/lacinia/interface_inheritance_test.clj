; Copyright (c) 2017-present Walmart, Inc.
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

(ns com.walmartlabs.lacinia.interface-inheritance-test
  "Tests related to interface inheritance functionality."
  (:refer-clojure :exclude [compile])
  (:require
    [clojure.test :refer [deftest is testing]]
    [com.walmartlabs.test-utils :refer [expect-exception]]
    [com.walmartlabs.lacinia.schema :refer [compile]]
    [com.walmartlabs.lacinia :refer [execute]]))

(def basic-inheritance
  '{:interfaces {:named {:fields {:name {:type String}}}
                 :person {:implements [:named]
                          :fields {:age {:type Int}}}}
    :objects {:employee {:implements [:person]
                         :fields {:name {:type String}
                                  :age {:type Int}
                                  :department {:type String}}}}})

(def multiple-inheritance
  '{:interfaces {:named {:fields {:name {:type String}}}
                 :aged {:fields {:age {:type Int}}}
                 :person {:implements [:named :aged]
                          :fields {:email {:type String}}}}
    :objects {:employee {:implements [:person]
                         :fields {:name {:type String}
                                  :age {:type Int}
                                  :email {:type String}
                                  :department {:type String}}}}})

(def circular-inheritance
  '{:interfaces {:a {:implements [:b]
                     :fields {:field_a {:type String}}}
                 :b {:implements [:a]
                     :fields {:field_b {:type String}}}}})

(def self-inheritance
  '{:interfaces {:a {:implements [:a]
                     :fields {:field_a {:type String}}}}})

(def nonexistent-parent
  '{:interfaces {:child {:implements [:nonexistent]
                         :fields {:field {:type String}}}}})

(def non-interface-parent
  '{:objects {:parent {:fields {:field {:type String}}}}
    :interfaces {:child {:implements [:parent]
                         :fields {:field {:type String}}}}})

(def incompatible-field-override
  '{:interfaces {:parent {:fields {:field {:type String}}}
                 :child {:implements [:parent]
                         :fields {:field {:type Int}}}}})

(deftest basic-interface-inheritance
  (testing "interface can implement another interface"
    (let [schema (compile basic-inheritance)]
      (is (some? schema)
          "Schema should compile successfully")
      (is (= #{:name :age} (-> schema :person :fields keys set))
          "Child interface should have fields from parent interface"))))

(deftest multiple-interface-inheritance
  (testing "interface can implement multiple interfaces"
    (let [schema (compile multiple-inheritance)]
      (is (some? schema)
          "Schema should compile successfully")
      (is (= #{:name :age :email} (-> schema :person :fields keys set))
          "Child interface should have fields from all parent interfaces"))))

(deftest circular-inheritance-detection
  (testing "circular inheritance is detected"
    (expect-exception
      "Interface `b' has a circular inheritance dependency."
      {:interface {:implements [:a]
                   :fields {:field_b {:type 'String}}
                   :category :interface
                   :type-name :b}}
      (compile circular-inheritance)))

  (testing "self inheritance is detected"
    (expect-exception
      "Interface `a' has a circular inheritance dependency."
      {:interface {:implements [:a]
                   :fields {:field_a {:type 'String}}
                   :category :interface
                   :type-name :a}}
      (compile self-inheritance))))

(deftest parent-interface-validation
  (testing "nonexistent parent interface"
    (expect-exception
      "Interface `child' extends interface `nonexistent', which does not exist."
      {:interface {:implements [:nonexistent]
                   :fields {:field {:type 'String}}
                   :category :interface
                   :type-name :child}
       :schema-types {:scalar [:Boolean :Float :ID :Int :String]
                      :object [:Mutation :Query :Subscription]
                      :interface [:child]}}
      (compile nonexistent-parent)))

  (testing "non-interface parent"
    (expect-exception
      "Interface `child' implements type `parent', which is not an interface."
      {:interface {:implements [:parent]
                   :fields {:field {:type 'String}}
                   :category :interface
                   :type-name :child}
       :schema-types {:scalar [:Boolean :Float :ID :Int :String]
                      :object [:Mutation :Query :Subscription :parent]
                      :interface [:child]}}
      (compile non-interface-parent))))

(deftest field-compatibility-validation
  (testing "incompatible field override"
    (expect-exception
      "Interface field is not compatible with parent interface type."
      {:parent-interface-name :parent
       :field-name :child/field}
      (compile incompatible-field-override))))

(defn employee-resolver
  [context args value]
  {:name "John"
   :age 30
   :department "Engineering"})

(def introspection-test-schema
  {:interfaces {:named {:fields {:name {:type 'String}}}
                :person {:implements [:named]
                         :fields {:age {:type 'Int}}}}
   :objects {:employee {:implements [:person]
                        :fields {:name {:type 'String}
                                 :age {:type 'Int}
                                 :department {:type 'String}}}}
   :queries {:employee {:type :employee
                        :resolve employee-resolver}}})

(deftest introspection-interface-inheritance
  (testing "introspection shows interface inheritance"
    (let [schema (compile introspection-test-schema)
          query "{ __type(name: \"person\") { name interfaces { name } } }"
          result (execute schema query nil nil)]
      (is (= {:data {:__type {:name "person"
                              :interfaces [{:name "named"}]}}}
             result)
          "Introspection should show interface inheritance"))))
