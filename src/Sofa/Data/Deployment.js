"use strict";

exports.getSfData = just => nothing => parsePageData => () => {
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
    crmQuoteId:
      typeof __QUOTE_SFID__ === "undefined" || __QUOTE_SFID__ === ""
        ? nothing
        : just(__QUOTE_SFID__),
    pageData:
      typeof __SALESFORCE_DATA__ === "undefined" || __SALESFORCE_DATA__ === ""
        ? nothing
        : just(parsePageData(__SALESFORCE_DATA__)())
  })
}
