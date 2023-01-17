Overview
========

GraphQL consists of two main parts:

* A server-side *schema* that defines the available queries and types of data that may be returned.

* A client query language that allows the client to specify what query to execute, and what data
  to return.

The `GraphQL specification <https://facebook.github.io/graphql>`_ goes into detail about the
format of the client query language, and the expected behavior of the server.

This library, Lacinia, is an implementation of the key component of the server,
in idiomatic Clojure.

Schema
------

The GraphQL specification includes a language to define the server-side schema; the
``type`` keyword is used to introduce a new kind of object.

In Lacinia, the schema is Clojure data: a map of keys and values; top level
keys indicate the type of data being defined:'

.. literalinclude:: /_examples/star-wars-schema.edn
   :language: clojure

Here, we are defining ``Human`` and ``Droid`` objects.
These have a lot in common, so we define a shared ``Character`` interface.

But how to access that data?  That's accomplished using one of three queries:

* hero

* human

* droid

In this example, each query returns a single instance of the matching object.
Often, a query will return a list of matching objects.

Compiling the Schema
--------------------

The schema defines the *shape* of the data that can be queried, but leaves out where that data comes from.
Unlike an object/relational mapping layer, where we might discuss database tables and rows, GraphQL (and
by extension, Lacinia) has *no* idea where the data comes from.

That's the realm of the :doc:`field resolver function <resolve/index>`.
Since EDN files are just data, we simply attach the
actual functions after the EDN data is read into memory.

The schema starts as a data structure, we need to add in the field resolvers and then *compile* the result.

.. literalinclude:: /_examples/compile-schema.clj
    :language: clojure

The :api:`util/inject-resolvers` function identifies objects and fields within those objects, and adds the resolver function. With those functions in place, the schema can be compiled for execution.

Compilation performs a number of checks, applies defaults, merges in introspection data about the schema,
and performs a number of other operations to ready the schema for use.
The structure passed into ``compile`` is quite complex, so it is always validated using
:doc:`clojure.spec <spec>`.


Parsing GraphQL IDL Schemas
---------------------------

Lacinia also offers support for parsing schemas defined in the `GraphQL Interface
Definition Language <https://github.com/facebook/graphql/pull/90>`_ and tranforming
them into the Lacinia schema data structure.

See :doc:`GraphQL IDL Schema Parsing <schema/parsing>` for details.

Executing Queries
-----------------

With that in place, we can now execute queries.

.. literalinclude:: _examples/overview-exec-query.edn
   :language: clojure

.. sidebar:: Ordered Map?

   The ``#ordered/map`` indicates that the fields in the result map are returned in the
   `same order` [#order]_ as they are specified in the query document.

   In most examples, for conciseness, a standard (unordered) map is shown.

The query string is parsed and matched against the queries defined in the schema.

The two nils are variables to be used executing the query, and an application context.

In GraphQL, queries can pass arguments (such as ``id``) and queries identify
exactly which fields
of the matching objects are to be returned.
This query can be stated as `just provide the name of the human with id '1001'`.

This is a successful query, it returns a result map [#result]_ with a ``:data`` key.
A failed query would return a map with an ``:errors`` key.
A query can even be partially successful, returning as much data as it can, but also errors.

Inside ``:data`` is a key corresponding to the query, ``:human``, whose value is the single
matching human.  Other queries might return a list of matches.
Since we requested just a slice of a full human object, just the human's name, the map has just a single
``:name`` key.

.. [#order] This shouldn't be strictly necessary (JSON and EDN don't normally care about key order, and
   keys can appear in arbitrary order),
   but having consistent ordering makes writing tests involving GraphQL queries easier: you can
   typically check the textual, not parsed, version of the result map directly against an expected string value.

.. [#result] In GraphQL's specification, this is referred to as the "response"; in practice,
   this result data forms the body of a response map (when using Ring or Pedestal). Lacinia
   uses the terms `result map` or `result data` to keep these ideas distinct.
