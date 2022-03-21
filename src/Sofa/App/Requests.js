"use strict";

exports.orderingBaseUrl =
  typeof __ORDERING_BASE_URL__ === 'undefined'
    ? ''
    : __ORDERING_BASE_URL__;

exports.smartSpecBaseUrl =
  typeof __SMART_SPEC_BASE_URL__ === 'undefined'
    ? ''
    : __SMART_SPEC_BASE_URL__;

exports.smartSpecProdCatalogFilename =
  typeof __DEPLOYMENT_ENV__ == 'undefined'
    ? 'product-catalog.json'
    : `product-catalog.${__DEPLOYMENT_ENV__}.n.json`;
