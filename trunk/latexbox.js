javascript:
popw='';
str=prompt("Type your tex:","%s");
if(str){
    popw=window.open(
        'http://www.forkosh.dreamhost.com/mathtex.cgi?'+escape(str),
        'latexbox',
        'width=300,height=200'
    );
    popw.focus();
    popd=popw.document;
    if (!document.all) T = setTimeout('popw.focus()',50)
}
void(0);