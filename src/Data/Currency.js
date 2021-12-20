"use strict";

exports.formatter = currency => {
  const format = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: currency,
    currencyDisplay: 'code',
    maximumFractionDigits: undefined
  });

  return number => format.format(number);
};

const numberFormat = new Intl.NumberFormat('en-US', {
  maximumFractionDigits: undefined
});

exports.numberFormatter = number => numberFormat.format(number);
