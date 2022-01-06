"use strict";

exports.formatToParts = currency => {
  const format = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: currency,
    maximumFractionDigits: undefined
  });

  return number => format.formatToParts(number);
};

exports.formatter = currency => {
  const format = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: currency,
    maximumFractionDigits: undefined
  });

  return number => format.format(number);
};

const numberFormat = new Intl.NumberFormat('en-US', {
  maximumFractionDigits: undefined
});

exports.numberFormatter = number => numberFormat.format(number);
