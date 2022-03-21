"use strict";

const BigNumber = require('bignumber.js');

exports.fromInt = n => new BigNumber(n)
exports.fromNumber = n => new BigNumber(n)

exports._fromString = just => nothing => n => {
  try {
    return just(new BigNumber(n))
  } catch (error) {
    return nothing
  }
}

exports.toNumber = n => n.toNumber()

exports.bigNumberToFixed = n => n.toFixed()
exports.bigNumberToFixedDp = dp => n => n.toFixed(dp)

exports.bigNumberEq = n1 => n2 => n1.isEqualTo(n2)

exports.bigNumberZero = new BigNumber(0)
exports.bigNumberOne = new BigNumber(1)
exports.bigNumberPlus = n1 => n2 => n1.plus(n2)
exports.bigNumberSub = n1 => n2 => n1.minus(n2)
exports.bigNumberMul = n1 => n2 => n1.multipliedBy(n2)
exports.bigNumberDiv = n1 => n2 => n1.dividedBy(n2)
exports.bigNumberMod = n1 => n2 => n1.modulo(n2)
