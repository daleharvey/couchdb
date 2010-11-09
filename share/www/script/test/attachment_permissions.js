couchTests.attachment_permissions = function(debug) {

  var dbName      = "test_suite_db", 
      usersDbName = "test_suite_users";

  var designDoc = {
    _id: "_design/" + dbName,
    _attachments:{
      "foo.txt": {
        content_type:"text/plain",
        data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    },
    views: {
      test: {map: "function(doc) { if (doc._id == \"1\") { emit(1, null) }}"}
    },
    shows: {
      test: "function() {return 'ok'};"
    },
    lists: {
      test: "function(head, req) { return 'ok'; }"
    }
  };

  var name = "dale", 
      pass = "top!secret";

  var createDb = function (name) { 
    var tmp = new CouchDB(name, {"X-Couch-Full-Commit":"false"});
    tmp.deleteDb();
    tmp.createDb();
    return tmp;
  };

  var securityObj = {
    readers : {names : ["johndoe"]}
  };
 

  var runTest = function () { 
    try { 

      var testDb = createDb(dbName);
      var usersDb = createDb(usersDbName);

      if (debug) debugger;
    
      T(testDb.save(designDoc).ok);
      T(usersDb.save(CouchDB.prepareUserDoc({name: name}, pass)).ok);
      usersDb.ensureFullCommit();
      T(testDb.setSecObj(securityObj).ok);
      T(CouchDB.login(name, pass).ok);

      var rootDb = "/" + dbName;
          dDoc   = rootDb + "/_design/" + dbName,
          attach = dDoc + "/foo.txt", 
          view   = dDoc + "/_view/test",
          list   = dDoc + "/_list/test/test",
          show   = dDoc + "/_show/test";

      T(CouchDB.request("GET", rootDb).status === 401);
      T(CouchDB.request("GET", dDoc).status === 401);
      T(CouchDB.request("GET", attach).status === 200);
      T(CouchDB.request("GET", list).status === 401);
      T(CouchDB.request("GET", show).status === 401);
      T(CouchDB.request("GET", view).status === 401);

    } finally { 
      CouchDB.logout();
    }
  };

  CouchDB.logout();

  run_on_modified_server([{
    section : "couch_httpd_auth",
    key     : "authentication_db", 
    value   : "test_suite_users"
  }, {
    section : "couch_httpd_auth",
    key     : "anonymous_design_doc", 
    value   : "true"
  }], runTest);
    
};