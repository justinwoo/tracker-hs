var Elm = require('./Main.elm');

var app = Elm.Main.embed(document.getElementById('app'), {
  showData: window.showData
});

app.ports.updateRequests.subscribe(function (x) {
  console.log(x)
  const xhr = new XMLHttpRequest();
  xhr.open('POST', x.target, true);
  xhr.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
  xhr.send('name=' + x.name);
  xhr.onload = function (e) {
    app.ports.showDataUpdates.send({
      name: x.name,
      count: e.target.response
    });
  };
});
