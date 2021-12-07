"use strict";

exports.orderingBaseUrl =
  typeof __ORDERING_BASE_URL__ === 'undefined'
    ? ''
    : __ORDERING_BASE_URL__;

exports.smartSpecBaseUrl =
  typeof __SMART_SPEC_BASE_URL__ === 'undefined'
    ? ''
    : __SMART_SPEC_BASE_URL__;
