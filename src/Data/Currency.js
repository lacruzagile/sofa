"use strict";

exports.formatter = currency => {
  var formatter = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: currency,
    currencyDisplay: 'code',
    maximumFractionDigits: undefined
  });

  return number => formatter.format(number);
};
