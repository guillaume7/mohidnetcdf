javascript:
(function twitterbox(){
  var h=location.href;
  var m=prompt("Your message:","%s. "+h);
  if(m){
    var u="guillaume7:guigui";
    var w=window.open("","twitterbox","width=400,height=100");
    var d=w.document;
    var f=d.createElement("form");
    f.setAttribute("name","twitter");
    f.setAttribute("method","POST");
    f.setAttribute("action","http://"+u+"@twitter.com/statuses/update.xml");
    var i=f.createElement("input");
    i.setAttribute("type","hidden");
    i.setAttribute("name","status");
    i.value=msg;
    f.submit();
  }
})();