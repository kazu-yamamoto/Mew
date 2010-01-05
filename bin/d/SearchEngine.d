module SearchEngine;

private import std.stdio;
private import std.string;
private import std.path;
private import Sqlite3DB;

////////////////////////////////////////////////////////////////

class SearchEngine {

////////////////////////////////////////////////////////////////
private:
  Sqlite3DB db;
  string myid;
  string mydir;

  Row[] selectById(string id) {
    string sql = format("SELECT * FROM mew WHERE (mew.id = '%s');", id);
    return db.execute(sql);
  }

  Row[] selectByParid(string[] ids) in {
    assert(ids.length > 0);
  } body {
    string cond = "";
    foreach (id; ids) {
      if (cond == "") 
	cond = format("mew.parid = '%s'", id);
      else
	cond ~= format("OR mew.parid = '%s'", id);
    }
    string sql = format("SELECT * FROM mew WHERE (%s);", cond);
    return db.execute(sql);
  }

  Row chooseOne(Row[] ents) {
    if (ents is null) return null;
    if (mydir is null) return ents[0];
    foreach (ent; ents) {
      if (dirname(ent["path"]) == mydir) {
	return ent;
      }
    }
    return ents[0];
  }

////////////////////////////////////////////////////////////////
public:
  this(string database, string myid, string mydir) {
    db = new Sqlite3DB();
    db.open(database);
    this.myid = myid;
    this.mydir = mydir;
  }

  ~this() {
    db.close();
  }

////////////////////////////////
  void searchMe() in {
    assert(myid != "");
  } body {
    Row[] ents = selectById(myid);
    if (ents is null) return;
    Row me = chooseOne(ents);
    writefln("%s", me["path"]);
  } 

////////////////////////////////  
  void searchChild() in {
    assert(myid != "");
  } body {
    Row[] ents = selectByParid([myid]);
    if (ents is null) return;
    Row child = chooseOne(ents);
    writefln("%s", child["path"]);
  }

////////////////////////////////
  void searchDescendants() {
    string rootid;
    int count = 0;
    int[string] index;
    Row[] descendants;

    string getParid(string id) {
      Row[] ents = selectById(id);
      if (ents is null) {
	return "";
      } else {
	return ents[0]["parid"];
      }
    }

    void findRoot() in {
      assert(myid != "");
    } body {
      rootid = myid;
      string parid = getParid(myid);
      while (parid != "") {
	rootid = parid;
	parid = getParid(parid);
      }
    }

    bool pushDescendant(Row ent) {
      int* ip = ent["id"] in index;
      if (ip is null) {
	index[ent["id"]] = count;
	descendants ~= ent;
	count++;
	return true;
      } else {
	if (dirname(ent["path"]) == mydir) {
	  Row old	= descendants[*ip];
	  delete(old);
	  descendants[*ip] = ent;
	}
	return false;
      }
    }

    void findDescendants() {
      Row root = chooseOne(selectById(rootid));
      pushDescendant(root);
      Row[] children = selectByParid([rootid]);
      while (children.length != 0) {
	string[] ids;
	foreach (child; children) {
	  if (pushDescendant(child))
	    ids ~= child["id"];
	}
	children = selectByParid(ids);
      }
    }

    void printDescendants() {
      descendants.sort;
      foreach (ent; descendants) {
	writefln("%s", ent["path"]);
      }
    }

    // the main part of searchDescendants
    findRoot();
    findDescendants();
    printDescendants();
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
