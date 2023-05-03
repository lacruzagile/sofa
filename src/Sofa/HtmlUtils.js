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

exports.uncheck = value => () => {
  var element = document.getElementById(value);
  element.checked = false
}

exports.uncheckall = value => () => {
  var inputs = document.querySelectorAll('input.menu');
  for (var i = 0; i < inputs.length; i++) {
    if(inputs[i].id !== value) {
      inputs[i].checked = false;
    }
  }
}

exports.clearCopyButtonsOnLoadMoreOrders = () => {
  var inputs = document.querySelectorAll('ul.submenu');
  for (var i = 0; i < inputs.length; i++) {
    var allDivs = inputs[i].firstChild.firstChild.childNodes
    var currentId = inputs[i].id.split('ul-')[1]
    var unique = false
    for(var j = 0; j < allDivs.length; j++){
      if(currentId !== allDivs[j].id.split('copy-order-')[1] || unique) {
        allDivs[j].remove();
        j--;
      } else if(currentId === allDivs[j].id.split('copy-order-')[1] && unique === false) {
        unique = true
      }
    }
  }
}

exports.reload = () => {
  location.reload()
}

