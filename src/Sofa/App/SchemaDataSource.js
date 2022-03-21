"use strict";

exports.replaceAllFun = p => f => s => {
  return s.replaceAll(p, f);
};
