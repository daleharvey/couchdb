// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

couchTests.new_replication = function(debug) {

  if (debug) debugger;

  var host = CouchDB.host;
  var sourceDb = new CouchDB("test_suite_db_a",{"X-Couch-Full-Commit":"false"});
  var targetDb = new CouchDB("test_suite_db_b",{"X-Couch-Full-Commit":"false"});

  var dbPairs = [
    {
      source: sourceDb.name,
      target: targetDb.name
    },
    {
      source: "http://" + host + "/" + sourceDb.name,
      target: targetDb.name
    },
    {
      source: sourceDb.name,
      target: "http://" + host + "/" + targetDb.name
    },
    {
      source: "http://" + host + "/" + sourceDb.name,
      target: "http://" + host + "/" + targetDb.name
    }
  ];

  var sourceInfo, targetInfo;
  var docs, doc, copy;
  var repResult;
  var i, j, k;


  function populateDb(db, docs, dontRecreateDb) {
    if (dontRecreateDb !== true) {
      db.deleteDb();
      db.createDb();
    }
    for (var i = 0; i < docs.length; i++) {
      var doc = docs[i];
      delete doc._rev;
    }
    if (docs.length > 0) {
      db.bulkSave(docs);
    }
  }


  // test simple replications (not continuous, not filtered), including
  // conflict creation
  docs = makeDocs(1, 21);
  docs.push({
    _id: "_design/foo",
    language: "javascript",
    value: "ddoc"
  });

  for (i = 0; i < dbPairs.length; i++) {
    populateDb(sourceDb, docs);
    populateDb(targetDb, []);

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);
    T(sourceInfo.update_seq === targetInfo.update_seq);

    T(typeof repResult.session_id === "string");
    T(repResult.source_last_seq === sourceInfo.update_seq);
    T(repResult.history instanceof Array);
    T(repResult.history.length === 1);
    T(repResult.history[0].session_id === repResult.session_id);
    T(typeof repResult.history[0].start_time === "string");
    T(typeof repResult.history[0].end_time === "string");
    T(repResult.history[0].start_last_seq === 0);
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === sourceInfo.doc_count);
    T(repResult.history[0].missing_found === sourceInfo.doc_count);
    T(repResult.history[0].docs_read === sourceInfo.doc_count);
    T(repResult.history[0].docs_written === sourceInfo.doc_count);
    T(repResult.history[0].doc_write_failures === 0);

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      T(copy !== null);
      for (var p in doc) {
        T(copy[p] === doc[p]);
      }
    }


    // add one more doc to source and replicate again
    var newDoc = {
      _id: "foo666",
      value: "d"
    };
    T(sourceDb.save(newDoc).ok);

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);
    T(sourceInfo.update_seq === targetInfo.update_seq);

    T(typeof repResult.session_id === "string");
    T(repResult.source_last_seq === sourceInfo.update_seq);
    T(repResult.history instanceof Array);
    T(repResult.history.length === 2);
    T(repResult.history[0].session_id === repResult.session_id);
    T(typeof repResult.history[0].start_time === "string");
    T(typeof repResult.history[0].end_time === "string");
    T(repResult.history[0].start_last_seq === (sourceInfo.update_seq - 1));
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === 1);
    T(repResult.history[0].missing_found === 1);
    T(repResult.history[0].docs_read === 1);
    T(repResult.history[0].docs_written === 1);
    T(repResult.history[0].doc_write_failures === 0);

    copy = targetDb.open(newDoc._id);
    T(copy !== null);
    T(copy._id === newDoc._id);
    T(copy.value === newDoc.value);

    // test deletion is replicated
    doc = sourceDb.open(docs[1]._id);
    T(sourceDb.deleteDoc(doc).ok);

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);
    T(sourceInfo.update_seq === targetInfo.update_seq);
    T(sourceInfo.doc_del_count === targetInfo.doc_del_count);
    T(targetInfo.doc_del_count === 1);

    T(repResult.history instanceof Array);
    T(repResult.history.length === 3);
    T(repResult.history[0].start_last_seq === (sourceInfo.update_seq - 1));
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === 1);
    T(repResult.history[0].missing_found === 1);
    T(repResult.history[0].docs_read === 1);
    T(repResult.history[0].docs_written === 1);
    T(repResult.history[0].doc_write_failures === 0);

    copy = targetDb.open(docs[1]._id);
    T(copy === null);

    var changes = targetDb.changes({since: sourceInfo.update_seq - 1});
    T(changes.results[0].id === docs[1]._id);
    T(changes.results[0].seq === sourceInfo.update_seq);
    T(changes.results[0].deleted === true);

    // test conflict
    doc = sourceDb.open(docs[0]._id);
    doc.value = "white";
    T(sourceDb.save(doc).ok);

    copy = targetDb.open(docs[0]._id);
    copy.value = "black";
    T(targetDb.save(copy).ok);

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);
    T(sourceInfo.update_seq === (targetInfo.update_seq - 1));

    T(repResult.history instanceof Array);
    T(repResult.history.length === 4);
    T(repResult.history[0].start_last_seq === (sourceInfo.update_seq - 1));
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === 1);
    T(repResult.history[0].missing_found === 1);
    T(repResult.history[0].docs_read === 1);
    T(repResult.history[0].docs_written === 1);
    T(repResult.history[0].doc_write_failures === 0);

    copy = targetDb.open(docs[0]._id, {conflicts: true});

    T(copy._rev.indexOf("2-") === 0);
    T(copy._conflicts instanceof Array);
    T(copy._conflicts.length === 1);
    T(copy._conflicts[0].indexOf("2-") === 0);

    // replicate again with conflict
    doc.value = "yellow";
    T(sourceDb.save(doc).ok);

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);
    T(sourceInfo.update_seq === (targetInfo.update_seq - 1));

    T(repResult.history instanceof Array);
    T(repResult.history.length === 5);
    T(repResult.history[0].start_last_seq === (sourceInfo.update_seq - 1));
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === 1);
    T(repResult.history[0].missing_found === 1);
    T(repResult.history[0].docs_read === 1);
    T(repResult.history[0].docs_written === 1);
    T(repResult.history[0].doc_write_failures === 0);

    copy = targetDb.open(docs[0]._id, {conflicts: true});

    T(copy._rev.indexOf("3-") === 0);
    T(copy._conflicts instanceof Array);
    T(copy._conflicts.length === 1);
    T(copy._conflicts[0].indexOf("2-") === 0);

    // resolve the conflict
    T(targetDb.deleteDoc({_id: copy._id, _rev: copy._conflicts[0]}).ok);

    // replicate again, check there are no more conflicts
    doc.value = "rainbow";
    T(sourceDb.save(doc).ok);

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);
    T(sourceInfo.update_seq === (targetInfo.update_seq - 2));

    T(repResult.history instanceof Array);
    T(repResult.history.length === 6);
    T(repResult.history[0].start_last_seq === (sourceInfo.update_seq - 1));
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === 1);
    T(repResult.history[0].missing_found === 1);
    T(repResult.history[0].docs_read === 1);
    T(repResult.history[0].docs_written === 1);
    T(repResult.history[0].doc_write_failures === 0);

    copy = targetDb.open(docs[0]._id, {conflicts: true});

    T(copy._rev.indexOf("4-") === 0);
    T(typeof copy._conflicts === "undefined");
  }


  // test create_target option
  for (i = 0; i < dbPairs.length; i++) {
    populateDb(sourceDb, docs);
    targetDb.deleteDb();

    repResult = CouchDB.new_replicate(
      dbPairs[i].source,
      dbPairs[i].target,
      {body: {create_target: true}}
    );
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);
    T(sourceInfo.update_seq === targetInfo.update_seq);
  }


  // test filtered replication
  docs = makeDocs(1, 31);
  docs.push({
    _id: "_design/mydesign",
    language: "javascript",
    filters: {
      myfilter: (function(doc, req) {
        var modulus = Number(req.query.modulus);
        var special = req.query.special;
        return (doc.integer % modulus === 0) || (doc.string === special);
      }).toString()
    }
  });

  for (i = 0; i < dbPairs.length; i++) {
    populateDb(sourceDb, docs);
    populateDb(targetDb, []);

    repResult = CouchDB.new_replicate(
      dbPairs[i].source,
      dbPairs[i].target,
      {
        body: {
          filter: "mydesign/myfilter",
          query_params: {
            modulus: 2,
            special: "7"
          }
        }
      }
    );

    T(repResult.ok === true);

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      if ((doc.integer && (doc.integer % 2 === 0)) || (doc.string === "7")) {

        T(copy !== null);
        for (var p in doc) {
          T(copy[p] === doc[p]);
        }
      } else {
        T(copy === null);
      }
    }

    T(repResult.history instanceof Array);
    T(repResult.history.length === 1);
    // NOT 31 (31 is db seq for last doc - the ddoc, which was not replicated)
    T(repResult.source_last_seq === 30);
    T(repResult.history[0].start_last_seq === 0);
    T(repResult.history[0].end_last_seq === 30);
    T(repResult.history[0].recorded_seq === 30);
    // 16 => 15 docs with even integer field  + 1 doc with string field "7"
    T(repResult.history[0].missing_checked === 16);
    T(repResult.history[0].missing_found === 16);
    T(repResult.history[0].docs_read === 16);
    T(repResult.history[0].docs_written === 16);
    T(repResult.history[0].doc_write_failures === 0);


    // add new docs to source and resume the same replication
    var newDocs = makeDocs(50, 56);
    populateDb(sourceDb, newDocs, true);

    repResult = CouchDB.new_replicate(
      dbPairs[i].source,
      dbPairs[i].target,
      {
        body: {
          filter: "mydesign/myfilter",
          query_params: {
            modulus: 2,
            special: "7"
          }
        }
      }
    );

    T(repResult.ok === true);

    for (j = 0; j < newDocs.length; j++) {
      doc = newDocs[j];
      copy = targetDb.open(doc._id);

      if (doc.integer && (doc.integer % 2 === 0)) {

        T(copy !== null);
        for (var p in doc) {
          T(copy[p] === doc[p]);
        }
      } else {
        T(copy === null);
      }
    }

    // last doc has even integer field, so last replicated seq is 36
    T(repResult.source_last_seq === 36);
    T(repResult.history instanceof Array);
    T(repResult.history.length === 2);
    T(repResult.history[0].start_last_seq === 30);
    T(repResult.history[0].end_last_seq === 36);
    T(repResult.history[0].recorded_seq === 36);
    T(repResult.history[0].missing_checked === 3);
    T(repResult.history[0].missing_found === 3);
    T(repResult.history[0].docs_read === 3);
    T(repResult.history[0].docs_written === 3);
    T(repResult.history[0].doc_write_failures === 0);
  }


  // test replication by doc IDs
  docs = makeDocs(1, 11);
  docs.push({
    _id: "_design/foo",
    language: "javascript",
    integer: 1
  });

  var target_doc_ids = [
    { initial: ["1", "2", "10"], after: [], conflict_id: "2" },
    { initial: ["1", "2"], after: ["7"], conflict_id: "1" },
    { initial: ["1", "foo_666", "10"], after: ["7"], conflict_id: "10" },
    { initial: ["_design/foo", "8"], after: ["foo_5"], conflict_id: "8" },
    { initial: [], after: ["foo_1000", "_design/foo", "1"], conflict_id: "1" }
  ];
  var doc_ids, after_doc_ids;
  var id, num_inexistent_docs, after_num_inexistent_docs;
  var total, after_total;

  for (i = 0; i < dbPairs.length; i++) {

    for (j = 0; j < target_doc_ids.length; j++) {
      doc_ids = target_doc_ids[j].initial;
      num_inexistent_docs = 0;

      for (k = 0; k < doc_ids.length; k++) {
        id = doc_ids[k];
        if (id.indexOf("foo_") === 0) {
          num_inexistent_docs += 1;
        }
      }

      populateDb(sourceDb, docs);
      populateDb(targetDb, []);

      repResult = CouchDB.new_replicate(
        dbPairs[i].source,
        dbPairs[i].target,
        {
          body: {
            doc_ids: doc_ids
          }
        }
      );

      total = doc_ids.length - num_inexistent_docs;
      T(repResult.ok === true);
      T(typeof repResult.start_time === "string");
      T(typeof repResult.end_time === "string");
      T(repResult.docs_read === total);
      T(repResult.docs_written === total);
      T(repResult.doc_write_failures === 0);

      targetInfo = targetDb.info();
      T(targetInfo.doc_count === total);

      for (k = 0; k < doc_ids.length; k++) {
        id = doc_ids[k];
        doc = sourceDb.open(id);
        copy = targetDb.open(id);

        if (id.indexOf("foo_") === 0) {
          T(doc === null);
          T(copy === null);
        } else {
          T(doc !== null);
          T(copy !== null);
          for (var p in doc) {
            T(copy[p] === doc[p]);
          }
        }
      }

      // add more docs throught replication by doc IDs
      after_doc_ids = target_doc_ids[j].after;
      after_num_inexistent_docs = 0;

      for (k = 0; k < after_doc_ids.length; k++) {
        id = after_doc_ids[k];
        if (id.indexOf("foo_") === 0) {
          after_num_inexistent_docs += 1;
        }
      }

      repResult = CouchDB.new_replicate(
        dbPairs[i].source,
        dbPairs[i].target,
        {
          body: {
            doc_ids: after_doc_ids
          }
        }
      );

      after_total = after_doc_ids.length - after_num_inexistent_docs;
      T(repResult.ok === true);
      T(typeof repResult.start_time === "string");
      T(typeof repResult.end_time === "string");
      T(repResult.docs_read === after_total);
      T(repResult.docs_written === after_total);
      T(repResult.doc_write_failures === 0);

      targetInfo = targetDb.info();
      T(targetInfo.doc_count === (total + after_total));

      for (k = 0; k < after_doc_ids.length; k++) {
        id = after_doc_ids[k];
        doc = sourceDb.open(id);
        copy = targetDb.open(id);

        if (id.indexOf("foo_") === 0) {
          T(doc === null);
          T(copy === null);
        } else {
          T(doc !== null);
          T(copy !== null);
          for (var p in doc) {
            T(copy[p] === doc[p]);
          }
        }
      }

      // replicate again the same doc after updated on source (no conflict)
      id = target_doc_ids[j].conflict_id;
      doc = sourceDb.open(id);
      T(doc !== null);
      doc.integer += 100;
      T(sourceDb.save(doc).ok);

      repResult = CouchDB.new_replicate(
        dbPairs[i].source,
        dbPairs[i].target,
        {
          body: {
            doc_ids: [id]
          }
        }
      );

      T(repResult.ok === true);
      T(repResult.docs_read === 1);
      T(repResult.docs_written === 1);
      T(repResult.doc_write_failures === 0);

      copy = targetDb.open(id, {conflicts: true});

      T(copy._rev.indexOf("2-") === 0);
      T(typeof copy._conflicts === "undefined");

      // generate a conflict throught replication by doc IDs
      id = target_doc_ids[j].conflict_id;
      doc = sourceDb.open(id);
      copy = targetDb.open(id);
      T(doc !== null);
      T(copy !== null);
      doc.integer += 100;
      copy.integer += 1;
      T(sourceDb.save(doc).ok);
      T(targetDb.save(copy).ok);

      repResult = CouchDB.new_replicate(
        dbPairs[i].source,
        dbPairs[i].target,
        {
          body: {
            doc_ids: [id]
          }
        }
      );

      T(repResult.ok === true);
      T(repResult.docs_read === 1);
      T(repResult.docs_written === 1);
      T(repResult.doc_write_failures === 0);

      copy = targetDb.open(id, {conflicts: true});

      T(copy._rev.indexOf("3-") === 0);
      T(copy._conflicts instanceof Array);
      T(copy._conflicts.length === 1);
      T(copy._conflicts[0].indexOf("3-") === 0);
    }
  }


  // cleanup
  sourceDb.deleteDb();
  targetDb.deleteDb();
}