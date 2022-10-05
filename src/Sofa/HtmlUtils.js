"use script";

exports.scrollToBottom = () => {
  var el = document.scrollingElement;
  el.scrollTop = el.scrollHeight;
}

exports.scrollIntoView = el => () => {
  el.scrollIntoView({behavior: "smooth"});
}

exports.copyToClipboard = value => () => {
  navigator.clipboard.writeText(value);
}

exports.back = () => {
  history.back()
}

exports.addClassToElement = value => name => () => {
  var element = document.getElementById(value);
  element.classList.add(name)
}

exports.removeClassToElement = value => name => () => {
  var element = document.getElementById(value);
  element.classList.remove(name)
}
