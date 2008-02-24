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

function embedimg(imglnk,imgalt) {

    var w=window.open(
        '',
        'imgbox',
        'width=800,height=600'
    )
    var d=w.document;
    
    var textimglnk=textarea(imglnk,'1','50');
    var imgtag=img(imglnk,imgalt);

}

var match=/<([^<>]*)>/g;
var subs="&lt;$1&gt;";
replace(match,subs);