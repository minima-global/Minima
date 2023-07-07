(function() {
  var el = document.getElementById('title-bar');

  if (el) {
    el.addEventListener('click', function() {
      if (window.navigator.userAgent.includes('Minima Browser')) {
        Android.showTitleBar();
      }
    });
  }
})();
