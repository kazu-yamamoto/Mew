module Sqlite3DB;

private import std.string: d2c = toStringz;
private import std.conv;

string c2d(char *s) {
  return to!string(s);
}

class Row {
  string[string] hash;

  string opIndex(string key) {
    return hash[key];
  }

  void opIndexAssign(string value, string key) {
    hash[key] = value;
  }

  int opCmp(Object obj) {
    Row row = cast(Row)obj;
    return hash["date"] > row.hash["date"];
  }
}

private struct sqlite3 {}
private struct sqlite3_stmt {}
private enum RC {
  OK,
  ROW = 100,
  DONE, 
}

extern (C) RC sqlite3_open(const(char)* filename, sqlite3** database);
extern (C) RC sqlite3_close(sqlite3* database);
extern (C) RC sqlite3_prepare(sqlite3* database, const(char)* zSql, int nByte, sqlite3_stmt** ppStmt, char** pzTail);
extern (C) RC sqlite3_step(sqlite3_stmt* pStmt);
extern (C) RC sqlite3_finalize(sqlite3_stmt* pStmt);
extern (C) int sqlite3_column_count(sqlite3_stmt* pStmt);
extern (C) char* sqlite3_column_name(sqlite3_stmt* pStmt, int N);
extern (C) char *sqlite3_column_text(sqlite3_stmt* pStmt, int iCol);

extern (C) char* sqlite3_errmsg(sqlite3* database);

class Sqlite3DB {
private:
  string file;
  sqlite3* database;
  sqlite3_stmt* stmt;

  void prepare(string sql) {
    char** errmsg;
    RC rc = sqlite3_prepare(database, d2c(sql), sql.length, &stmt, errmsg);
    if (rc != RC.OK) {
	throw new Exception("sqlite3_prepare: " ~ c2d(sqlite3_errmsg(database)));
    }
  }

  RC step() {
    RC rc = sqlite3_step(stmt);
    if (rc != RC.ROW && rc != RC.DONE) {
	throw new Exception("sqlite3_step: " ~ c2d(sqlite3_errmsg(database)));
    }
    return rc;
  }

  void finalize() {
    RC rc = sqlite3_finalize(stmt);
    if (rc != RC.OK) {
	throw new Exception("sqlite3_finalize: " ~ c2d(sqlite3_errmsg(database)));
    }
  }

  Row result() {
    Row ret = new Row();
    for (int i = 0; i < sqlite3_column_count(stmt); i++) {
      ret[c2d(sqlite3_column_name(stmt, i))]
	= c2d(sqlite3_column_text(stmt, i));
    }
    return ret;
  }

public:
  Row[] execute(string sql) in {
    assert(database !is null);
  } out {
    assert(database !is null);
  } body {
    Row[] results;
    prepare(sql);
    while (step() != RC.DONE) {
      results ~= result();
    }
    finalize();
    return results;
  }

  void open(string file) in {
    assert(database is null);
  } out {
    assert(database !is null);
  } body {
    this.file = file;
    RC rc = sqlite3_open(d2c(file), &database);
    if (rc != RC.OK) {
	throw new Exception("sqlite3_open: " ~ c2d(sqlite3_errmsg(database)));
    }
  }

  void close() in {
    assert(database !is null);
  } out {
    assert(database is null);
  } body {
    RC rc = sqlite3_close(database);
    if (rc != RC.OK) {
      throw new Exception("sqlite3_close: " ~ c2d(sqlite3_errmsg(database)));
    }
    database = null;
  }
}

/* 
 * Copyright (C) 2008 Mew developing team.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the team nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
