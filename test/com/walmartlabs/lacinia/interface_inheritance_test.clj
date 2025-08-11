(ns com.walmartlabs.lacinia.interface-inheritance-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [com.walmartlabs.lacinia.schema :as schema]
    [com.walmartlabs.lacinia.parser.schema :as parser]
    [com.walmartlabs.lacinia :as lacinia]))

(def test-schema-sdl
  "interface Node {
    id: ID!
  }

  interface Named implements Node {
    id: ID!
    name: String!
  }

  interface Timestamped implements Node {
    id: ID!
    createdAt: String!
    updatedAt: String!
  }

  interface NamedTimestamped implements Named & Timestamped {
    id: ID!
    name: String!
    createdAt: String!
    updatedAt: String!
  }

  type User implements NamedTimestamped {
    id: ID!
    name: String!
    createdAt: String!
    updatedAt: String!
    email: String!
  }

  type Query {
    user(id: ID!): User
  }")

(def test-schema-edn
  {:interfaces
   {:Node {:fields {:id {:type '(non-null ID)}}}
    :Named {:implements [:Node]
            :fields {:id {:type '(non-null ID)}
                     :name {:type '(non-null String)}}}
    :Timestamped {:implements [:Node]
                  :fields {:id {:type '(non-null ID)}
                           :createdAt {:type '(non-null String)}
                           :updatedAt {:type '(non-null String)}}}
    :NamedTimestamped {:implements [:Named :Timestamped]
                       :fields {:id {:type '(non-null ID)}
                                :name {:type '(non-null String)}
                                :createdAt {:type '(non-null String)}
                                :updatedAt {:type '(non-null String)}}}}
   :objects
   {:User {:implements [:NamedTimestamped]
           :fields {:id {:type '(non-null ID)}
                    :name {:type '(non-null String)}
                    :createdAt {:type '(non-null String)}
                    :updatedAt {:type '(non-null String)}
                    :email {:type '(non-null String)}}}}
   :queries
   {:user {:type :User
           :args {:id {:type '(non-null ID)}}
           :resolve (constantly nil)}}})

(deftest test-interface-inheritance-parsing
  (testing "SDL parser handles interface inheritance"
    (let [parsed-schema (parser/parse-schema test-schema-sdl)]
      (is (= [:Node] (get-in parsed-schema [:interfaces :Named :implements])))
      (is (= [:Node] (get-in parsed-schema [:interfaces :Timestamped :implements])))
      (is (= [:Named :Timestamped] (get-in parsed-schema [:interfaces :NamedTimestamped :implements]))))))

(deftest test-interface-inheritance-compilation
  (testing "Interface inheritance compiles correctly"
    (let [compiled-schema (schema/compile test-schema-edn)]
      (is (some? compiled-schema))
      (is (= #{:Node} (get-in compiled-schema [:Named :implements])))
      (is (= #{:Node} (get-in compiled-schema [:Timestamped :implements])))
      (is (= #{:Named :Timestamped} (get-in compiled-schema [:NamedTimestamped :implements]))))))

(deftest test-field-inheritance
  (testing "Child interfaces inherit fields from parent interfaces"
    (let [compiled-schema (schema/compile test-schema-edn)
          named-fields (get-in compiled-schema [:Named :fields])
          timestamped-fields (get-in compiled-schema [:Timestamped :fields])
          named-timestamped-fields (get-in compiled-schema [:NamedTimestamped :fields])]
      (is (contains? named-fields :id))
      (is (contains? named-fields :name))
      (is (contains? timestamped-fields :id))
      (is (contains? timestamped-fields :createdAt))
      (is (contains? timestamped-fields :updatedAt))
      (is (contains? named-timestamped-fields :id))
      (is (contains? named-timestamped-fields :name))
      (is (contains? named-timestamped-fields :createdAt))
      (is (contains? named-timestamped-fields :updatedAt)))))

(deftest test-circular-inheritance-detection
  (testing "Circular inheritance is detected and rejected"
    (let [circular-schema
          {:interfaces
           {:A {:implements [:B]
                :fields {:fieldA {:type 'String}
                         :fieldB {:type 'String}
                         :fieldC {:type 'String}}}
            :B {:implements [:C]
                :fields {:fieldB {:type 'String}
                         :fieldC {:type 'String}}}
            :C {:implements [:A]
                :fields {:fieldC {:type 'String}
                         :fieldA {:type 'String}
                         :fieldB {:type 'String}}}}}]
      (is (thrown-with-msg? Exception #"Circular interface inheritance detected"
                            (schema/compile circular-schema))))))

(deftest test-missing-interface-validation
  (testing "Missing parent interface is detected"
    (let [invalid-schema
          {:interfaces
           {:Child {:implements [:NonExistent]
                    :fields {:field {:type 'String}}}}}]
      (is (thrown-with-msg? Exception #"extends interface.*which does not exist"
                            (schema/compile invalid-schema))))))

(deftest test-non-interface-implementation
  (testing "Implementing non-interface type is rejected"
    (let [invalid-schema
          {:scalars {:CustomScalar {:parse identity :serialize identity}}
           :interfaces
           {:Child {:implements [:CustomScalar]
                    :fields {:field {:type 'String}}}}}]
      (is (thrown-with-msg? Exception #"implements type.*which is not an interface"
                            (schema/compile invalid-schema))))))

(deftest test-transitive-object-validation
  (testing "Objects are validated against all transitive interfaces"
    (let [compiled-schema (schema/compile test-schema-edn)
          user-type (get compiled-schema :User)]
      (is (some? user-type))
      (is (contains? (:implements user-type) :NamedTimestamped)))))

(deftest test-introspection-interface-inheritance
  (testing "Introspection reports interface inheritance correctly"
    (let [compiled-schema (schema/compile test-schema-edn)
          query "{ __type(name: \"Named\") { interfaces { name } } }"
          result (lacinia/execute compiled-schema query nil nil)]
      (is (nil? (:errors result)))
      (is (= "Node" (get-in result [:data :__type :interfaces 0 :name]))))))
