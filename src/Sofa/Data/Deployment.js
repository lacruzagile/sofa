"use strict";

exports.sfData = just => nothing => () => {
  if (typeof __ACCESSTOKEN__ === "undefined"
      || typeof __ORGANIZATION_ID__ === "undefined"
      || typeof __USER_ID__ === "undefined"
      || typeof __USER_EMAIL__ === "undefined") {
    return nothing;
  }

  return just({
    accessToken:  __ACCESSTOKEN__,
    organizationId:  __ORGANIZATION_ID__,
    userId:  __USER_ID__,
    userEmail:  __USER_EMAIL__,
  })
}

exports._getCrmQuoteId = just => nothing => () => {
  if (typeof __QUOTE_SFID__ === "undefined" || __QUOTE_SFID__ == "") {
    return nothing;
  } else {
    return just(__QUOTE_SFID__);
  }
}
