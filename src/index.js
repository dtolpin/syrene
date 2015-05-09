// $Id: index.js,v 1.2 2007/01/18 07:53:01 dvd Exp $

var explanation, trprev = undefined;

function set_explanator(block) {
  var tr = block.parentNode.parentNode;
  tr.addEventListener("mouseover",
                      function(evt) {
                        if(trprev)
                          trprev.className = "case";
                        tr.className = "current case";
                        trprev = tr;
                        if(explanation.firstChild)
                          explanation.removeChild(explanation.firstChild);
                        explanation.appendChild(block.firstChild.cloneNode(false));
                      },false);
}

window.addEventListener
  ("load",
   function(evt) {
     var blocks = document.getElementsByTagName("block");
     explanation = document.getElementById("explanation");
     for(var i=0; i!=blocks.length; ++i) {
       var block = blocks[i];
       if(block.className=="explatext")
         set_explanator(block);
     }
   }, 
   false);