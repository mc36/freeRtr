window.onload = function () {
  var term = new Terminal();
  term.open(document.getElementById('terminal'));
  var image = new ImageAddon.ImageAddon();
  var fit = new FitAddon.FitAddon();
  term.loadAddon(image);
  term.loadAddon(fit);
  fit.fit();
  term.focus();
  var url = document.URL;
  var i = url.lastIndexOf(".");
  var o = url.lastIndexOf("/");
  if (o > i) { i = o; }
  o = url.indexOf("/");
  url = "ws:" + url.substring(o, i);
  term.write("connecting " + url + " ...\r\n\r\n");
  var websock = new WebSocket(url, ['binary']);
  var attach = new AttachAddon.AttachAddon(websock);
  term.loadAddon(attach);
  window.addEventListener('resize', function () { fit.fit(); });
};
