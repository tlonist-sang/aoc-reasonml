// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

var arr = Belt_Array.map(Fs.readFileSync("input/q6in.txt", "utf8").trim().split("\t"), Caml_format.caml_int_of_string);

function create_node(existOpt, nextOpt, param) {
  var exist = existOpt !== undefined ? existOpt : false;
  var next = nextOpt !== undefined ? nextOpt : Belt_Array.make(20, {
          next: [],
          exist: false
        });
  return {
          next: next,
          exist: exist
        };
}

function distribute(arr) {
  var length = arr.length;
  var max = Pervasives.min_int;
  var max_index = 0;
  for(var i = 0; i < length; ++i){
    var v = Belt_Array.get(arr, i);
    if (v !== undefined && v > max) {
      max = v;
      max_index = i;
    }
    
  }
  var toAdd = Caml_int32.div(max, length - 1 | 0);
  arr[max_index] = 0;
  if (toAdd === 0) {
    var max_index_ = max_index;
    for(var i$1 = max_index_ + 1 | 0; i$1 < length; ++i$1){
      var e = Belt_Array.get(arr, i$1);
      if (e !== undefined) {
        arr[i$1] = e + 1 | 0;
      }
      
    }
    max = max - (((length - 1 | 0) - max_index_ | 0) - 1 | 0) | 0;
    for(var i$2 = 0; i$2 <= max_index_; ++i$2){
      var e$1 = Belt_Array.get(arr, i$2);
      if (e$1 !== undefined) {
        arr[i$2] = e$1 + 1 | 0;
      }
      
    }
    return arr;
  }
  var added_arr = arr.map(function (i) {
        return i + toAdd | 0;
      });
  added_arr[max_index] = max - Math.imul(toAdd, length - 1 | 0) | 0;
  return added_arr;
}

function save(_index, arr, _node) {
  while(true) {
    var node = _node;
    var index = _index;
    if (index >= arr.length) {
      node.exist = true;
      return node.exist;
    }
    var v = Belt_Array.get(arr, index);
    if (v === undefined) {
      return false;
    }
    var w = Belt_Array.get(node.next, v);
    if (w !== undefined) {
      console.log("save some=>", w);
      _node = w;
      _index = index + 1 | 0;
      continue ;
    }
    console.log("save none, making new. v =>", v);
    var inserted = create_node(undefined, undefined, undefined);
    node.next[v] = inserted;
    var w$1 = Belt_Array.get(node.next, v);
    if (w$1 === undefined) {
      return false;
    }
    _node = w$1;
    _index = index + 1 | 0;
    continue ;
  };
}

function check_trie(_index, arr, _node) {
  while(true) {
    var node = _node;
    var index = _index;
    if (index >= arr.length) {
      if (node.exist === true) {
        return true;
      } else {
        return false;
      }
    }
    var v = Belt_Array.get(arr, index);
    if (v === undefined) {
      return false;
    }
    var w = Belt_Array.get(node.next, v);
    if (w === undefined) {
      return false;
    }
    _node = w;
    _index = index + 1 | 0;
    continue ;
  };
}

function part1(_arr, _step, node) {
  while(true) {
    var step = _step;
    var arr = _arr;
    console.log("step=>", step);
    var distributed_array = distribute(arr);
    if (check_trie(0, distributed_array, node) === true) {
      console.log(distributed_array);
      return step + 1 | 0;
    }
    save(0, distributed_array, node);
    _step = step + 1 | 0;
    _arr = distributed_array;
    continue ;
  };
}

var init_node = create_node(undefined, undefined, undefined);

save(0, [
      0,
      2,
      7,
      0
    ], init_node);

console.log("hello world!");

console.log(check_trie(0, [
          2,
          2,
          7,
          0
        ], init_node));

exports.arr = arr;
exports.create_node = create_node;
exports.distribute = distribute;
exports.save = save;
exports.check_trie = check_trie;
exports.part1 = part1;
exports.init_node = init_node;
/* arr Not a pure module */