"use script";

exports.scrollToBottom = () => {
  var el = document.scrollingElement;
  el.scrollTop = el.scrollHeight;
}
