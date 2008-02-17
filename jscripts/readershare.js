javascript:
var l=document.getElementsByTagName("link");
var href="";
for(i=0;i<l.length;i++){
    type=l[i].getAttribute("type");
    rel=l[i].getAttribute("rel");
    if(rel&&type&&rel=="alternate"&&type.indexOf("+xml")!=-1){
        href=l[i].getAttribute("href");
        if(href.indexOf("http")!=0){
            var k=href;var loc=window.document.location;
            if(href.indexOf("/")!=0){
                var j=loc.pathname.split("/");
                j[j.length-1]=href;
                k=j.join("/")
            }
            href=loc.protocol+"//"+loc.hostname+href;
        }
        break;
    }
}
if(href!=""){
    var txt="";
    if(window.getSelection){
        txt=window.getSelection();
    }
    else if(document.getSelection){
        txt=document.getSelection();
    }
    else if(document.selection){
        txt=document.selection.createRange().text;
    };
    var query;
    if(txt!="")query=txt;
    else query=document.title;
    var gread="http://www.google.com/reader/view/#search/"+query+"//"+encodeURIComponent("feed/"+href);
    window.document.location=gread;
}
else{
    alert("No available feed!");
}