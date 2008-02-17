function Hash() {
    this.length = 0;
    this.items = new Array();
    for (var i = 0; i < arguments.length; i += 2) {
        if (typeof(arguments[i + 1]) != 'undefined') {
            this.items[arguments[i]] = arguments[i + 1];
            this.length++;
        }
    }
   
    this.removeItem = function(in_key) {
        var tmp_value;
        if (typeof(this.items[in_key]) != 'undefined') {
            this.length--;
            var tmp_value = this.items[in_key];
            delete this.items[in_key];
        }
       
        return tmp_value;
    }

    this.getItem = function(in_key) {
        return this.items[in_key];
    }

    this.setItem = function(in_key, in_value) {
        if (typeof(in_value) != 'undefined') {
            if (typeof(this.items[in_key]) == 'undefined') {
                this.length++;
            }

            this.items[in_key] = in_value;
        }
       
        return in_value;
    }

    this.hasItem = function(in_key) {
        return typeof(this.items[in_key]) != 'undefined';
    }
}

function embedimg(imglnk,imgalt) {

    var match=/<([^<>]*)>/g;
    var subs="&lt;$1&gt;";

    var popw=window.open(
        '',
        'imgbox',
        'width=800,height=600'
    )
    var popd=popw.document;
    
    var textimglnk=textarea(imglnk,'1','50');
    var imgtag=img(imglnk,imgalt);

}

document.makeElement = function(in_hash) {
    var element = this.createElement('');
    for (var i in in_hash.items) {
        this.element.setAttribute(i, in_hash.getItem(i));
    }
    return element;
}

function textarea(nodeValue,rows,cols) {
    var myHash = new Hash(
        'nodeType', 'textarea',
        'nodeValue', nodeValue,
        'rows',rows,
        'cols',cols,
    );
    return document.makeElement(myHash);
}

function img(src,alt) {
    var myHash = new Hash(
        'nodeType', 'img',
        'src',lnk,
        'alt',alt,
    );
    return document.makeElement(myHash);
}

function div(nodeValue) {
    var myHash = new Hash(
        'nodeType', 'div',
        'nodeValue', nodeValue,
    )
    return document.makeElement(myHash);
}

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
