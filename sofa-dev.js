var Main = require('./output/Main');

// See https://parceljs.org/features/development#hot-reloading.
if (module.hot) {
  module.hot.accept(function () {
    console.log('Reloaded, running main again');
    document.body.replaceChildren();
    Main.main();
  });
}

console.log('Starting app');

Main.main();
