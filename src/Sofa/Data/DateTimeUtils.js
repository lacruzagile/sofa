"use strict";

exports._fromIsoString = just => nothing => str => {
  try {
    return just(new Date(str))
  } catch (error) {
    return nothing
  }
}

exports._toIsoString = d => d.toISOString()
