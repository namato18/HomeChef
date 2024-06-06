// www/cookie-handler.js

Shiny.addCustomMessageHandler("setCookie", function(message) {
  var expires = "";
  if (message.expires) {
    var date = new Date();
    date.setTime(date.getTime() + (message.expires * 24 * 60 * 60 * 1000));
    expires = "; expires=" + date.toUTCString();
  }
  document.cookie = message.name + "=" + (message.value || "")  + expires + "; path=/";
});

Shiny.addCustomMessageHandler("getCookie", function(message) {
  var nameEQ = message.name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1,c.length);
    if (c.indexOf(nameEQ) == 0) {
      Shiny.setInputValue("cookie", {name: message.name, value: c.substring(nameEQ.length,c.length)});
      return;
    }
  }
  Shiny.setInputValue("cookie", {name: message.name, value: ""});
});

Shiny.addCustomMessageHandler("clearCookie", function(message) {
  document.cookie = message.name + "=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;";
  console.log('cookie cleared')
});