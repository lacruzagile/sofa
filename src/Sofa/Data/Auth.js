"use strict";

exports.ssoBaseUrl = () => {
  if (typeof __DEPLOYMENT_ENV__ == 'undefined' || __DEPLOYMENT_ENV__ == 'staging') {
    return 'https://sso.admin-portal.int.staging.sinch.com'
  } else {
    throw `unknown deployment environment ${__DEPLOYMENT_ENV__}`
  }
}
