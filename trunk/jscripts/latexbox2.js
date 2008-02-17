javascript:
popw='';
str=prompt("Type your tex:","%s");
cols=50;
match=/<([^<>]*)>/g;
subs="&lt;$1&gt;";
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
    textdivtag='<textarea rows="3" cols="'+cols+'">'+divtag.replace(match,subs)+'</textarea>';
    popd.write(imgtag+'</br>');
    popd.write('link: '+textimglnk+'</br>');
    popd.write('html: '+textdivtag+'</br>');
    popw.focus();
    if (!document.all) T = setTimeout('popw.focus()',50)
}
void(0);
