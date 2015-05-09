/* $Id: form.js,v 1.24 2007/05/04 09:28:29 dvd Exp $ */

/* JavaScript library used by questionnaire. */

function compatibility_hackery() {
  if(!Array.prototype.some) {
    Array.prototype.some =  function(f) {
      for(var i=0; i!=this.length; ++i) {
        if(f(this[i])) return true;
      }
      return false;
    }
  }
  
  if(!Array.prototype.map) {
    Array.prototype.map = function(f) {
      var a = new Array();
      for(var i=0; i!=this.length; ++i) {
        a.push(f(this[i]));
      }
      return a;
    }
  }
}
compatibility_hackery();


function tag_name(node) {
  return node.tagName.toLowerCase();
}

function text_content(node) {
  if(node.textContent!=undefined)
    return node.textContent;
  else
    return node.childNodes[0].nodeValue;
}

function attach_uncheckers_to_radios() {
  /* when the user clicks on a checked radio, uncheck it */
  var inputs = document.getElementsByTagName("input");

  for(var i=0;i!=inputs.length; ++i) {
    var node = inputs[i];
    if(node.attributes["type"].value=="radio") {
      node.addEventListener
        ("mousedown",
         function(evt) {
           this.prev_checked = this.checked;
         },
         false);
      node.addEventListener
        ("click",
         function(evt) {
           this.checked = !this.prev_checked;
         },
         false);
    }
  }
}

function bind_checkboxes () {
  /* for every checkbox, link its onchange handler to the paired
     hidden field */
  var inputs = document.getElementsByTagName("input"),
      hidtab = {};

  function linkhidden(hidden) {
    return function(evt) {
      switch(hidden.value) {
      case "0":
      case "1": hidden.value = "2"; break;
      case "2": hidden.value = "1"; break;
      }
    }
  }

  for(var i=0; i!=inputs.length; ++i) {
    if(inputs[i].attributes["type"].value=="hidden")
      hidtab[inputs[i].name] = inputs[i];
  }

  for( var i=0; i!=inputs.length; ++i) {
    if(inputs[i].attributes["type"].value=="checkbox") {
      inputs[i].addEventListener("click",
                                 linkhidden(hidtab[inputs[i].name]),false);
    }
  }
}

// evidence & syndrome value retrieval 
var memotab = {};

function memoize(f) {
  /* a dictionary of memoized syndrome values is attached
     to the current window, every value is computed once
     per pass */
  return function(x) {
    if(x in memotab) return memotab[x];
    else return memotab[x] = f(x);
  }
}
 
var value = memoize
  (function(name) {
    /* retrieve variable value from the document. */
    var syval = syndrome(name,value); /* syndrome computation is
                                         defined elsewhere */
    if(syval!=undefined) return syval;
    var byname = document.getElementsByName(name);
    
    for(var i=0; i!=byname.length; ++i) {
      var node = byname[i];
      if(tag_name(node)=="input") {
        switch(node.attributes["type"].value) {
        case "text": return node.value==""?0:1;
        case "hidden": return parseFloat(node.value);
        case "radio":
          if(node.checked)
            return parseFloat(node.value);
          else
            break;
        }
      }
    }

    return 0;
  });

syndrome = function (name,value) {
  if(name in syndromes) return syndromes[name](value);
  return undefined;
}

function reshape_subtree(node) {
  /* for every node, get its children. for
     div[@class=section-reference] insert section.
     for div[@class=expansion] traverse children and expand
     those that qualify. For the rest, just call
     reshape_quesionnaire(child). */

  function selected(val,div) {
    /* tests whether the division is selected */
    var guards = new Array();
    for(var i=0;i!=div.childNodes.length; ++i) {
      var node = div.childNodes[i];
      if(node.nodeType==1
         && tag_name(node)=="span"
         && node.className=="guard") {
        guards.push(node);
      }
    }

    return guards
      .some(function(guard) {
          var exp = text_content(guard),
            op = exp.substring(0,2),
            arg = exp.substring(2);
          switch(op) {
          case "**": return true;
          case "eq": return val==parseFloat(arg);
          case "le": return val<=parseFloat(arg);
          case "ge": return val>=parseFloat(arg);
          case "in":
            var bounds = arg.split(' ').map(parseFloat);
            return bounds[0]<=val && val <= bounds[1];
          }
        });
  }

  if(node.nodeType==1) {
    if(tag_name(node)=="div" && node.className=="section-reference") {
      var sectId;
      for(var i = 0; i!=node.childNodes.length; ++i) {
        var child = node.childNodes[i];
        if(child.nodeType==1
           && tag_name(child)=="span"
           && child.className=="ref-id") {
          sectId = child.childNodes[0].nodeValue;
          break;
        }
      }
      var section = document.getElementById(sectId);
      node.appendChild(section);
      reshape_subtree(section);
    } else if(tag_name(node)=="div" && node.className=="expansion") {
      var val;
      for(var i=0; i!= node.childNodes.length; ++i) {
        var child = node.childNodes[i];
        if(child.nodeType==1) {
          if(tag_name(child)=="span"
             && child.className=="ref-name") {
            val = value(text_content(child));
          } else if(tag_name(child)=="div"
                    && child.className=="selection") {
            if(selected(val,child)) {
              child.style.display = "block";
              reshape_subtree(child);
            } else {
              child.style.display = "none";
            }
          }
        }
      }
    } else {
      for(var i = 0; i!=node.childNodes.length; ++i) {
        reshape_subtree(node.childNodes[i]);
      }
    }
  }
}

function mark_and_sweep (node) {
  /* reset hidden input elements to defaults */
  var active_inputs = {}, s = "", t = "";
  
  function mark(node) {
    if(node.nodeType==1) {
      if(node.style.display!="none") {
        if(tag_name(node)=="input") {
          active_inputs[node.name] = true;
        }
        for(var i=0;i!=node.childNodes.length; ++i) {
          mark(node.childNodes[i]);
        }
      }
    }
  }

  function reset(input) {
    /* reset input element to default (unanswered) state; must be
       in agreement with Form.lhs:instance Ht Context Answer */
    switch(input.attributes["type"].value) {
    case "text": input.value = ""; break;
    case "hidden": if(input.className=="hidcheck") input.value = "0"; break;
    case "radio": case "checkbox": input.checked = false; break;
    }
  }

  mark(node);
  
  var inputs = document.getElementsByTagName("input");
  for(var i=0;i!=inputs.length;++i) {
    s+= inputs[i].name+" ";
    if(!(inputs[i].name in active_inputs)) {
      t+= inputs[i].name+" ";
      reset(inputs[i]);
    }
  }
}

function reshape_questionnaire() {
  memotab = {};
  var soil = document.getElementById("se--soil");
  reshape_subtree(soil);
  mark_and_sweep(soil);
}

function reshape_on_input() {
  var inputs = document.getElementsByTagName("input");
  for(var i=0;i!=inputs.length; ++i) {
    node = inputs[i];
    var event;
    switch(node.attributes["type"].value) {
    case "radio":
    case "checkbox": event = "click"; break;
    case "text": event = "change"; break;
    }
    node.addEventListener(event,reshape_questionnaire,false);
  }
}

function activate_alternative_explanations() {
  var expladiv = document.getElementById("explanation"),
      as = document.getElementsByTagName("a"),
      onclick =
        function (evt) {
          if(expladiv.firstChild) expladiv.removeChild(expladiv.firstChild);
          expladiv.appendChild(this.getElementsByTagName("block")[0].firstChild.cloneNode(false));
        };
  for(var i=0;i!=as.length;++i) {
    node = as[i];
    if(node.className=='explalink') 
      node.addEventListener("click",onclick,false);
  }
}

function hide_compatibility_banner() {
  var banner = document.getElementById("loading-banner");
  banner.parentNode.removeChild(banner);
}

window.addEventListener
  ("load",
   function (evt) {
     attach_uncheckers_to_radios(document.body);
     bind_checkboxes(document.body);
     reshape_questionnaire();
     reshape_on_input();
     activate_alternative_explanations();
     hide_compatibility_banner();
   },
   false);
