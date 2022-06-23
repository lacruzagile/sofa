"use strict";

exports.ssoBaseUrl = () => {
  const urlStaging = 'https://sso.admin-portal.int.staging.sinch.com';
  const urlProduction = 'https://sso.int.clxnetworks.net';

  // If we are deployed in Kubernetes then determine the environment by simply checking the URL.
  if (typeof __DEPLOYMENT_ENV__ == 'undefined') {
    if (window.location.hostname == 'sofa.eu1.bpa.unauth.int.prod.sinch.com') {
      return urlProduction;
    } else {
      return urlStaging;
    }
  } else if (__DEPLOYMENT_ENV__ == 'staging') {
    return urlStaging;
  } else if (__DEPLOYMENT_ENV__ == 'production') {
    return urlProduction;
  } else {
    throw `unknown deployment environment ${__DEPLOYMENT_ENV__}`
  }
}
