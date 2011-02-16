# Quickstart example

Chillax is [Quicklisp](http://quicklisp.org)-installable, allowing for super-quick, painless
installation of Chillax and all its dependencies.

Make sure CouchDB is installed, and currently running. This example assumes that the server is
running in localhost, using the default 5984 port. Yason's alist encoder/decoder is used below to
make replies readable (by default, it uses hash tables as JSON objects, instead of alists, and
strings as keys).

    CL-USER> (ql:quickload 'chillax)
             ...
             ...
             ...
    CL-USER> (in-package :chillax)
    CHILLAX> (defparameter *server* (make-instance 'yason-server :object-as-alist-p t
                                                   :parse-object-key-fun
                                                   (lambda (string) (intern string *package*))))
    *SERVER*
    CHILLAX> (defparameter *db* (ensure-db *server* "test"))
    *DB*
    CHILLAX> (all-documents *db*)
    ((|rows|) (|offset| . 0) (|total_rows| . 0))
    CHILLAX> (put-document *db* "my_test_doc" '((name . "Josh") (favorite-language . "common-lisp")))
    ((|rev| . "1-3c964cc898c03c48903a91d90b24a269")
     (|id| . "my_test_doc") (|ok| . T))
    CHILLAX> (get-document *db* "my_test_doc")
    ((FAVORITE-LANGUAGE . "common-lisp")
     (NAME . "Josh")
     (|_rev| . "1-3c964cc898c03c48903a91d90b24a269")
     (|_id| . "my_test_doc"))
    CHILLAX> (delete-document *db* (cdr (assoc '|_id| *)) (cdr (assoc '|_rev| *)))
    ((|rev| . "2-2221fc2b97c1fac1a82ba07d2835ac80")
     (|id| . "my_test_doc")
     (|ok| . T))

# Introduction

Chillax is a [CouchDB](http://couchdb.apache.org) abstraction layer for Common Lisp. It is **not** an
ORM or similar library, although it may be used to build such things.

Chillax also includes a CouchDB view server, which can be used to write CouchDB views in full,
native Common Lisp.

Chillax includes several systems:

* chillax.asd - This is a 'DWIM' system. It includes the Yason server for parsing/encoding JSON
  data. It also defines a single `#:chillax` package that includes the symbols in both
  `#:chillax.core` and `#:chillax.yason`. If you don't know what you want, **this is probably what
  you want**.
* chillax.core.asd - Core API and protocols for servers, databases, documents, and
  design-docs.
* chillax.yason.asd - Implementation of the server protocol using Yason's JSON parser.
* chillax.jsown.asd - Implementation of the server protocol using JSOWN's JSON parser.
* chillax.utils.asd - Some handy utilities.
* chillax.view-server.asd - The Chillax view server. This only depends on chillax.utils.

# About this document

This README is not meant to document how to use CouchDB itself, or how to successfully design
applications with it. If you're new to CouchDB, it is recommended that you follow a guide (which can
be done while using Chillax).

The [CouchDB Guide](http://guide.couchdb.org/) is recommended, and the
[CouchDB Wiki](http://wiki.apache.org/couchdb/) can be used as a general reference for CouchDB's
API.

# Core API

Chillax is a thin, lispy layer on top of CouchDB's RESTful API. Its main purpose is to provide Lisp
functions to all of CouchDB's API calls, while translating certain things into Lisp data and
concepts. For example, Chillax takes care of checking CouchDB's HTTP response codes for sanity. When
error codes are returned, Chillax will signal Lisp corresponding Lisp conditions.

Additionally, Chillax is able to use any representation for CouchDB documents, provided the core
Chillax protocols are implemented for that representation, i.e, you can use hash tables, alists, or
instances of classes as your 'documents', and Chillax will automatically serialize/deserialize JSON
communications with CouchDB according to the protocol's implementation.

Note that, since Chillax really is just a thin layer, it abstracts very little beyond the
above. Most Chillax functions will simply return the de-JSON-ified CouchDB responses, and leave you
to digging through the response objects for the data you need.

## Server API

Chillax's Server API provides access to server-level functionality. Server objects not only give
access to these database-independent features, but also act as mediators for all data that goes
through Chillax. Chillax's server objects are in charge, for example, of translating CouchDB's JSON
responses to and from whatever data format the user has chosen to use Lisp-side.

Server objects are created through appropriate protocol implementations. See STANDARD-SERVER, and
YASON-SERVER below for the included implementations. Additional or alternate functionality for
serialization can easily be implemented through Chillax's server protocol.

*[function]* `server-uri server`

  Returns a string representation of the URL SERVER represents.


*[function]* `all-dbs server`

  Requests a list of all existing databases from SERVER.


*[function]* `config-info server`

  Requests the current configuration from SERVER.


*[function]* `replicate server target &key create-target-p continuousp`

  Replicates the database in SOURCE to TARGET. SOURCE and TARGET can both be either database names
  in the local server, or full URLs to local or remote databases. If CREATE-TARGET-P is true, the
  target database will automatically be created if it does not exist. If CONTINUOUSP is true,
  CouchDB will continue propagating any changes in SOURCE to TARGET.


*[function]* `stats server`

  Requests general statistics from SERVER.


*[function]* `active-tasks server`

  Lists all the currently active tasks on SERVER.


*[function]* `get-uuids server &key (number 10)`

  Returns a list of NUMBER unique IDs requested from SERVER. The UUIDs generated by the server are
  reasonably unique, but are not checked against existing UUIDs, so conflicts may still happen.


## Database API

Connecting to a database in Chillax involves creating a database object with one of 3 constructors,
all of which accept a server object (see Server API above), and the name of the database.

The Database API additionally provides some utility functions for accessing database-level
functionality in CouchDB.

Wiki: [HTTP Database API](http://wiki.apache.org/couchdb/HTTP_database_API)

*[function]* `db-connect server name`

  Confirms that a particular CouchDB database exists. If so, returns a new database object that can
  be used to perform operations on it. Will signal a DB-NOT-FOUND error if the database does not
  already exist.


*[function]* `db-create server name`

  Creates a new CouchDB database. Returns a database object that can be used to operate on it. Will
  signal a DB-ALREADY-EXISTS error if there is already a database with the same NAME in SERVER.


*[function]* `ensure-db server name`

  Either connects to an existing database, or creates a new one. Returns two values: If a new
  database was created, (DB-OBJECT T) is returned. Otherwise, (DB-OBJECT NIL).


*[function]* `db-info db`

  Fetches info about a given database from the CouchDB server.


*[function]* `db-delete db`

  Deletes a CouchDB database.


*[function]* `db-compact db`

  Triggers a database compaction.


*[function]* `db-changes db`

  Returns the changes feed for DB


*[function]* `db-uri db`

  Returns a string representing the full URI for DB.


## Document API

All document arguments to these functions must be Lisp objects that the database's server is able to
encode to JSON. These functions will likewise return Lisp objects that represent the parsed JSON
responses from CouchDB. Exact representations will depend on the server being used.

Wiki: [HTTP Document API](http://wiki.apache.org/couchdb/HTTP_Document_API)

*[function]* `get-document db id &key attachmentsp (errorpt)`

  Finds a CouchDB document in DB, named by ID. PARAMS should be an alist containing the parameters
for the HTTP GET request. If ATTACHMENTSP is TRUE, the document's attachments will be included in
their entirety in their base64-encoded version. It is not recommended you use this unless you really
know what you're doing. If ERRORP is NIL, GET-DOCUMENT will simply return NIL on 404.


*[function]* `get-document-revision db doc-id &key (errorp t)`

  Quickly fetches the latest revision for DOC-ID. If ERRORP is NIL, this can be used to quickly test
  the existence of a document.


*[function]* `put-document db id doc &key batch-ok-p`

  Puts a document into DB, using ID. DOC must be Lisp data that DB's server is able to convert to
  JSON.


*[function]* `post-document db doc`

  POSTs a document into DB. CouchDB will automatically assign a UUID if the document does not
  already exist. Note that using this function is discouraged in the CouchDB documentation, since it
  may result in duplicate documents because of proxies and other network intermediaries. If what you
  need is to create a new document with a generated id, consider using GET-UUIDS with PUT-DOCUMENT.


*[function]* `delete-document db id revision`

  Deletes an existing document.


*[function]* `copy-document from-id to-id &key revision`

  Copies a document's content in-database.


## Bulk Document API

CouchDB supports a bulk document API for fetching, updating, and deleting documents in batches.

Wiki: [HTTP Bulk Document API](http://wiki.apache.org/couchdb/HTTP_Bulk_Document_API)

*[function]* `all-documents db &rest all-keys`

  Requests the \_all\_docs document. ALL-KEYS correspond to GET-DOCUMENT's keyword arguments.


*[function]* `batch-get-documents db &rest doc-ids`

  Uses \_all\_docs to quickly fetch the given DOC-IDs in a single request. Note that this function
  will NOT signal a DOCUMENT-NOT-FOUND error when one or more DOC-IDs are not found. Instead, the
  results will be returned, and it's the user's responsibility to deal with any missing docs.


*[function]* `bulk-post-documents documents &key all-or-nothing-p`

  Allows you to update or submit multiple documents at the same time, using CouchDB's \_bulk\_docs
  API. In order to delete a document through this API, the document must have a \_document attribute
  with JSON 'true' as its value (note that what gets translated into 'true' depends on the server).

  DOCUMENTS must be a sequence or sequence-like (depending on what DATA->JSON will do to it).

  If ALL-OR-NOTHING-P is true, the entire submission will fail if a single one fails."


## Standalone Attachment API

CouchDB has an API for uploading and downloading standalone attachments.

Wiki: [Standalone Attachments](http://wiki.apache.org/couchdb/HTTP_Document_API#Standalone_Attachments)

*[function]* `put-attachment db doc-id attachment-name data &key rev content-type`

  Adds DATA as an attachment. DATA can be a number of things:

  * String or sequence of octets - DATA will be sent as-is directly to the server (using EXTERNAL-FORMAT-OUT for strings).
  * Stream - The stream will be read until EOF is reached.
  * Pathname - The file the pathname denotes will be opened and its data uploaded.
  * Function designator - The corresponding function will be called with one argument, the stream to the server, to which it should send data.

  If the document already exists, REV is required. This function can be used on non-existent
  documents. If so, REV is not needed, and a document will be created automatically, and the
  attachment associated with it.

  The CONTENT-TYPE should be a string specifying the content type for DATA. (default: "application/octet-stream")


*[function]* `get-attachment db doc-id attachment-name`

  Returns 3 values:

  1. STREAM - An open flexi-stream that can be READ. In order to read straight binary data, you must first fetch the underlying stream with FLEXI-STREAMS:FLEXI-STREAM-STREAM.
  2. MUST-CLOSE-P - A boolean. If TRUE, the user must CLOSE this stream themselves once reading is done.
  3. CONTENT-LENGTH - Declared content length for the incoming data.


*[function]* `delete-attachment db doc-id attachment-name doc-revision`

  Deletes an attachment from a document. DOC-REVISION must be the latest revision for the document.


*[function]* `copy-attachment db doc-id attachment-name output-stream`

  Copies data from the named attachment to OUTPUT-STREAM. Returns the number of bytes copied.


## Design Document API

Chillax currently includes a basic wrapper for Design Document-related operations. This API is
likely to expand in the future as good ideas reveal themselves. Design documents will still need to
be created through the regular document API.

Wiki: [HTTP View API](http://wiki.apache.org/couchdb/HTTP_view_API)

*[function]* `view-cleanup db`

  Invokes \_view\_cleanup on DB. Old view output will remain on disk until this is invoked.


*[function]* `compact-design-doc db design-doc-name`

  Compaction can really help when you have very large views, very little space, or both.


*[function]* `design-doc-info db design-doc-name`

  Returns an object with various bits of status information. Refer to CouchDB documentation for
  specifics on each value.


*[function]* `query-view db design-doc-name view-name &key ... (see below) ...`

  Queries view named by VIEW-NAME in DESIGN-DOC-NAME. Keyword arguments correspond to CouchDB view
  query arguments.

  * key - Single key to search for.
  * multi-keys - Multiple keys to search for.
  * startkey - When searching for a range of keys, the key to start from.
  * endkey - When searching for a range of keys, the key to end at. Whether this is inclusive or not depends on inclusive-end-p (default: true)
  * inclusive-end-p - If TRUE, endkey is included in the result. (default: true)
  * startkey-docid - Like startkey, but keyed on the result documents' doc-ids.
  * endkey-docid - Like endkey, but keyed on the result documents' doc-ids.
  * limit - Maximum number of results to return.
  * stalep - If TRUE, CouchDB will not refresh the view, even if it is stalled. (default: false)
  * descendingp - If TRUE, will return reversed results. (default: false)
  * skip - Number of documents to skip while querying.
  * groupp - Controls whether the reduce function reduces to a set of distinct keys, or to a single result row.
  * group-level - It's complicated. Google it!
  * reducep - If FALSE, return the view without applying its reduce function (if any). (default: true)
  * include-docs-p - If TRUE, includes the entire document with the result of the query. (default: false)


*[function]* `query-temporary-view db &key (map (error)) reduce (language "javascript")`

  Queries a temporary view. These views are meant to be for testing and development purposes, and
  should _not_ be used in actual code.


# Core Protocol

You can use the core protocol to customize behavior of Chillax. Writing your own implementation for
this protocol is relatively simple and painless, and it allows you to do things such as use your own
custom JSON encoder/decoder, if Yason doesn't fit your needs. Classes that implement the protocols,
and their behavior, is documented here, as well.

For more information on the design of Chillax's protocol, refer to
[Chillax and Protocols](http://sykosomatic.org/blog/?p=92).

## Server Protocol

Self-explanatory readers:

* *[generic function]* `server-host server`
* *[generic function]* `server-port server`
* *[generic function]* `server-username server`
* *[generic function]* `server-password server`
* *[generic function]* `server-secure-p server`


*[generic function]* `data->json server data &key`

  Converts DATA to JSON suitable for sending to CouchDB.


*[generic function]* `json->data server json &key`

  Converts JSON to the desired data structure.


*[generic function]* `make-db-object server name`

  Creates an object which represents a database connection in SERVER. The object must conform to the
  database protocol.


### Included protocol implementations

*[standard class]* `standard-server`

  Basic implementation of the server protocol. JSON data is handled literally as strings, with no
  conversion.

  It supports the following initargs:

  * :host - Host or IP address, in string form, of the CouchDB server. (default: "127.0.0.1")
  * :port - Port, as an integer, for the CouchDB server. (default: 5984)
  * :username - Username to use to authenticate with CouchDB server. (default: nil)
  * :password - Password to use to authenticate with CouchDB server. (default: nil)
  * :securep - Whether to use a secure SSL/TLS connection with the server. (default: nil)


*[standard class]* `yason-server`

  YASON-SERVERs use Yason's JSON parser/encoder to automatically translate content going to/coming
  from the associated CouchDB server.

  YASON-SERVER is a subclass of STANDARD-SERVER, and the same initargs apply.

  It additionally supports the following initargs:

  * :array-as-vector-p - If TRUE, parses JSON arrays as Lisp vectors. (default: nil)
  * :boolean-as-symbol-p - If TRUE, parses JSON booleans as symbols instead of CL booleans. (default: nil)
  * :object-as-alist-p - If TRUE, parses JSON objects as alists, instead of hash tables. (default: nil)
  * :parse-object-key-fun - Function to process object keys with. (default: #'cl:identity)


*[standard class]* `jsown-server`

   JSOWN-SERVERs use JSOWN's JSON parser/encoder to automatically translate content going to/coming
   from the associated CouchDB server.

   JSOWN-SERVER is a subclass of STANDARD-SERVER, and the same initargs apply.


*[function]* `call-with-key-container`

  This function is part of the chillax.jsown package.

  Calls FUNCTION, which require zero arguments, in a context where CONTAINER will be used for
  jsown:parse-with-container.


*[macro]* `with-key-container`

  This macro is part of the chillax.jsown package.

  Convenience macro for call-with-key-container.


## Database Protocol

*[generic function]* `database-server database`

  Returns the server object with which DATABASE is associated.


*[generic function]* `database-name database`

  Returns the URL-encoded name of the database, a string. Note that CouchDB accepts certain
  characters in database names -only- if they are URL-encoded (such as #\/). It is up to individual
  implementations of DATABASE-NAME to implement this encoding.


### Included protocol implementation

*[standard class]* `standard-database`

  Minimal, class-based implementation of the database protocol.


# View Server

The Chillax View Server allows you to write views in full-fledged Common Lisp. In order to use the
server, you must create a binary using code loaded by chillax.view-server.asd, and make
chillax-server::run-server the toplevel function.

Once you put the binary in a place visible to the CouchDB process, add the following to
/etc/couchdb/local.ini (or wherever your local.ini is located):

    [query_servers]
    common-lisp = /path/to/chillax-view-server

Add the common-lisp entry if there's already a query\_servers entry.

Once you've done this, you can start making queries directly in Lisp!

    CHILLAX> (invoke-temporary-view *db* :language "common-lisp"
                                         :map (prin1-to-string '(lambda (doc) (emit doc 1))))

You can load anything you want into the view server image before dumping it, customize which package
it executes views in, etc. The source code is fairly short and easy to digest. It's designed to be
customizable for whatever your needs are.

## Status

Currently, the view server only provides basic features. A future release of Chillax will bring it
back in sync with CouchDB's current line protocol.

# Yason Problem

Note that, at least on some systems, versions of [Yason](http://github.com/hanshuebner/Yason) prior
to commit
[00b9a5c06b7c4113a48518a1f136637efb4458b9](http://github.com/hanshuebner/Yason/commit/00b9a5c06b7c4113a48518a1f136637efb4458b9)
will not work (in this commit, #\Return was added to the list of whitespace characters
recognized). Using these versions instead of 0.1 is recommended anyway for performance reasons.

# History

The original author and current maintainer/developer of Chillax is
[Josh Marchán](http://github.com/sykopomp); [Ian McEwen](http://github.com/ianmcorvidae)
subsequently worked on CLOS bindings, since the original version was written for
[Sheeple](http://github.com/sykopomp/sheeple). Chillax has since been mostly rewritten (again) to
use a simple protocol-based extension API, allowing users to easily extend or alter Chillax's
behavior, with minimal code.

# Contributors

* Josh Marchán <sykopomp at sykosomatic.org>
* Adlai Chandrasekhar <munchking at gmail.com>
* Ian McEwen <ianmcorvidae at ianmcorvidae.net>
* Felix Lange <fjl at twurst.com>
