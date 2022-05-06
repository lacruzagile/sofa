"use script";

exports.scrollToBottom = () => {
  var el = document.scrollingElement;
  el.scrollTop = el.scrollHeight;
}

exports.scrollIntoView = el => () => {
  el.scrollIntoView({behavior: "smooth"});
}
