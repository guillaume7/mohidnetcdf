javascript:
(function(){
  var tex=prompt("TeX input:","");
  if(tex){
    w=window.open(
        "http://www.forkosh.dreamhost.com/mathtex.cgi?"+encodeURIComponent(tex),
        "latexbox",
        "width=300,height=200"
    );
    d=w.document;
    w.focus();
  }
}
)();

javascript:
popw='';
str=prompt("Type your tex:","%s");
cols=50;
if(str){
    popw=window.open(
        '',
        'latexbox',
        'width=440,height=250'
    );
    popd=popw.document;
    imglnk='http://www.forkosh.dreamhost.com/mathtex.cgi?'+escape(str);
    textimglnk='<textarea rows="1" cols="'+cols+'">'+imglnk+'</textarea>';
    imgtag='<img src="'+imglnk+'" alt="'+str+'"></img>';
    divtag='<div style="text-align:center;width:100%" >'+imgtag+'</div>';
    textdivtag='<textarea rows="1" cols="'+cols+'">'+divtag+'</textarea>';
    popd.write(imgtag+'</br>');
    popd.write('link: '+textimglnk+'</br>');
    popd.write('html: '+textdivtag+'</br>');
    popw.focus();
    if (!document.all) T = setTimeout('popw.focus()',50)
}
void(0);

   function toCharEnt() {
    var mtoc = /<([^<>]*)>/g;
    var box1 = document.convert.textbox1;
    var box2 = document.convert.textbox2;
    box2.value = box1.value.replace(mtoc,"&lt;$1&gt;");
    if ( document.convert.withPre.checked )
       box2.value = "<pre>\n" + box2.value + "\n</pre>"
   }

   function toMarkup() {
    var ctom = /&lt;([^&]*)&gt;/g;
    var box1 = document.convert.textbox1;
    var box2 = document.convert.textbox2;
    box1.value = box2.value.replace(ctom,"<$1>");
   }