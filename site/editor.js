import {EditorView, keymap, highlightSpecialChars, drawSelection, highlightActiveLine, dropCursor,
        rectangularSelection, crosshairCursor,
        lineNumbers, highlightActiveLineGutter} from "@codemirror/view"
import {Extension, EditorState,Text} from "@codemirror/state"
import {defaultHighlightStyle, syntaxHighlighting, indentOnInput, bracketMatching,
        foldGutter, foldKeymap} from "@codemirror/language"
import {defaultKeymap, history, historyKeymap} from "@codemirror/commands"
import {searchKeymap, highlightSelectionMatches} from "@codemirror/search"
import {autocompletion, completionKeymap, closeBrackets, closeBracketsKeymap} from "@codemirror/autocomplete"
import {lintKeymap} from "@codemirror/lint"

import {StreamLanguage} from "@codemirror/language"
import {haskell} from "@codemirror/legacy-modes/mode/haskell"

import $ from "jquery"
import AU from "ansi_up"

import {emptyTemplate,overflowExample,sumExample,constraintSetup,randomSetup} from "./templates.js"

let ansi_up = new AU();

const setupLines = (constraintSetup.match(/\n/g) || "").length + 1;

const basicSetup = [
  // lineNumbers(),
  highlightSpecialChars(),
  history(),
  foldGutter(),
  drawSelection(),
  dropCursor(),
  EditorState.allowMultipleSelections.of(true),
  indentOnInput(),
  syntaxHighlighting(defaultHighlightStyle, {fallback: true}),
  bracketMatching(),
  closeBrackets(),
  autocompletion(),
  rectangularSelection(),
  crosshairCursor(),
  highlightSelectionMatches(),
  keymap.of([
    ...closeBracketsKeymap,
    ...defaultKeymap,
    ...searchKeymap,
    ...historyKeymap,
    ...foldKeymap,
    ...completionKeymap,
    ...lintKeymap
  ])
  ,StreamLanguage.define(haskell)
]

const highlightActive = [highlightActiveLine(),highlightActiveLineGutter()]

let setupView = new EditorView({
  doc: constraintSetup,
  extensions:
    [basicSetup
    ,lineNumbers({formatNumber : n => '\xa0\xa0' + n })
    ,EditorView.editable.of(false)
  ],
  parent: document.getElementById("setupEditor")
})



let srcView = new EditorView({
  doc: sumExample,
  extensions:
    [basicSetup,highlightActive
    ,lineNumbers({formatNumber: n => n+setupLines})
    ,StreamLanguage.define(haskell)
  ],
  parent: document.getElementById("srcEditor")
});

// setup buttons
document.getElementById("btn-constraints").addEventListener("click",loadSetup(constraintSetup));
document.getElementById("btn-constraints").addEventListener("click",setupType(true));

document.getElementById("btn-random").addEventListener("click",loadSetup(randomSetup));
document.getElementById("btn-random").addEventListener("click",setupType(false));

document.getElementById("btn-empty").addEventListener("click",loadExample(emptyTemplate));
document.getElementById("btn-sum").addEventListener("click",loadExample(sumExample));
document.getElementById("btn-overflow").addEventListener("click",loadExample(overflowExample));
document.getElementById("compile-button").addEventListener("click",sendSrc);

let setupWithConstraint = true;
function setupType(val) {
  let f = () => {
    setupWithConstraint = val
    console.log(setupWithConstraint);
  }
  return f
}

function loadSetup(src) {
  let f = () => {
    let len = setupView.state.doc.length
    setupView.dispatch({changes: {from: 0, to:len, insert: src}})
  }
  return f
}

function loadExample(src) {
  let f = () => {
    let len = srcView.state.doc.length
    srcView.dispatch({changes: {from: 0, to:len, insert: src}})
  }
  return f
}

function sendSrc() {
  let ws = new WebSocket("ws://localhost:8080/")
  let output = document.getElementById("output");
  output.innerHTML="";

  ws.onopen = () => {
    let src = srcView.state.doc.toString();
    ws.send("send_src")
    if (setupWithConstraint) {
      ws.send("constraints")
    } else {
      ws.send("random")
    }
    ws.send(src)
    ws.send("EOF")
  }

  ws.onmessage = evt => {
    let str = evt.data + "\n";

    // test if output should be cleared
    if (str.search(/tests\)/) >= 0) {
      output.innerHTML = "";
    } else if (str.search(/generated/) >= 0) {
      output.innerHTML = "";
    } else if (str.search(/\+\+\+/) && !str.search(/generated/) >= 0) {
      output.innerHTML = "";
    } else if (output.innerHTML.search(/compiling/) >= 0) {
      output.innerHTML = "";
    }

    output.innerHTML += ansi_up.ansi_to_html(str);
  }

  window.onbeforeunload = evt => {
    socket.close()
  }
};
