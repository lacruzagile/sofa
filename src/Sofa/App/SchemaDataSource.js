"use strict";

exports.replaceAllFun = pat => fun => str => {
  return () => str.replaceAll(pat, fun);
};
