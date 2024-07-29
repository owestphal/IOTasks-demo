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

import { emptyTemplate
       , productExample
       , sumExample
       , sumExampleWithOutput
       , sumToZero
       , singlePath
       , fullTree
       , stringExample
       , untilValidExample
       , greeter
       , palindrom
       , constraintSetup
       , randomSetup
    } from "./templates.js"

import {websocketURL} from "./server-data.js"

import {initTerminal} from "./terminal.js"

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
    ,lineNumbers({formatNumber : n => '\xa0' + n })
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
    ,EditorView.updateListener.of(updateRecompileInfo)
  ],
  parent: document.getElementById("srcEditor")
});

let term = initTerminal()

// app state and constants
const output = document.getElementById("output")
const appState = {
  ws : null,
  armed : false,
  setupWithConstraint : true,
  showMore : false,
  moreSMT : true,
  busy : false,
  sendConstraints : null,
  smtProblems : null,
  currentProblem : 0,
  needsRecompile : false,
  lastCompiledSrc : null,
  runMethod : defaultRunMethod,
}

function defaultRunMethod() {
  if (appState.armed) {
    output.textContent = "please choose an artifact to run"
  }
}

function setOverflowStatus(val) {
  if (val) {
    document.getElementById("overflow").classList.remove("hidden")
    document.getElementById("no-overflow").classList.add("hidden")
  } else {
    document.getElementById("overflow").classList.add("hidden")
    document.getElementById("no-overflow").classList.remove("hidden")
  }
}

function setArmed(val, onclose=false) {
  appState.armed = val
  if (!onclose) {
    output.textContent=""
    document.getElementById("output-more").textContent=""
  }
  if (val) {
    // reset need for recompile
    updateRecompileInfo(false)
    // show armed controls
    document.getElementById("run-button").classList.remove("grayscale")
    document.getElementById("artifact-menu").classList.remove("grayscale")
    document.getElementById("div-input").classList.remove("grayscale")
    document.getElementById("div-smt").classList.remove("grayscale")
  } else {
    if (!onclose) {
      if (appState.ws != null && appState.ws.readyState < 2) {
        appState.ws.close()
      }
      appState.ws = null
    }
    appState.lastCompiledSrc = null
    // hide armed controls
    document.getElementById("run-button").classList.add("grayscale")
    document.getElementById("artifact-menu").classList.add("grayscale")
    document.getElementById("div-input").classList.add("grayscale")
    document.getElementById("div-smt").classList.add("grayscale")
  }
}

function updateRecompileInfo(update = null) {
  let src = srcView.state.doc.toString();
  let isNewSrc = src != appState.lastCompiledSrc
  let modeChanged = appState.setupWithConstraint != appState.sendConstraints
  setRecompileNeeded(appState.armed && (modeChanged || isNewSrc))
}

function setupConstraints(val) {
    appState.setupWithConstraint = val
    if (val) {
      document.getElementById("btn-constraints").classList.add("ring")
      document.getElementById("btn-random").classList.remove("ring")
    } else {
      document.getElementById("btn-constraints").classList.remove("ring")
      document.getElementById("btn-random").classList.add("ring")
    }
    updateRecompileInfo()
}

const moreText = "More"
document.getElementById("btn-more").textContent=moreText+" ▶"
function toggleMore() {
  if (appState.showMore) {
    document.getElementById("div-input").classList.add("hidden")
    document.getElementById("div-smt").classList.add("hidden")
    document.getElementById("btn-more").textContent=moreText+" ▶"
  } else {
    document.getElementById("div-input").classList.remove("hidden")
    document.getElementById("div-smt").classList.remove("hidden")
    document.getElementById("btn-more").textContent=moreText+" ◀"
  }
  appState.showMore = !appState.showMore;
}

function setMoreSMT(val) {
  appState.moreSMT = val
  if (val) {
    document.getElementById("btn-smt-code").classList.add("ring")
    document.getElementById("btn-sample-input").classList.remove("ring")
  } else {
    document.getElementById("btn-smt-code").classList.remove("ring")
    document.getElementById("btn-sample-input").classList.add("ring")
  }
}

function setSendConstraints(val) {
  appState.sendConstraints = val
  updateMoreView()
}

function setTerminalVisibility(val) {
  if (val) {
    document.getElementById("console").classList.remove("hidden")
    document.getElementById("output-with-controls").classList.add("hidden")
  } else {
    document.getElementById("console").classList.add("hidden")
    document.getElementById("output-with-controls").classList.remove("hidden")
  }
}

function updateMoreView() {
  document.getElementById("smt-controls").classList.add("hidden")
  if (appState.sendConstraints) {
    document.getElementById("btn-smt-code").classList.remove("hidden")
    document.getElementById("path-length").classList.remove("hidden")
  } else {
    document.getElementById("btn-smt-code").classList.add("hidden")
    document.getElementById("path-length").classList.add("hidden")
  }
}

function updateProblemDispaly() {
  document.getElementById("smt-controls").classList.remove("hidden")

  document.getElementById("problem-count").textContent = String(appState.currentProblem+1) + "/" +appState.smtProblems.length
  document.getElementById("output-more").textContent = appState.smtProblems[appState.currentProblem]
}

function setRecompileNeeded(val) {
  appState.needsRecompile = val
  if (val) {
    document.getElementById("recompile-needed").classList.remove("hidden")
  } else {
    document.getElementById("recompile-needed").classList.add("hidden")
  }
}

function setRunContext(ctx) {
  let f = () => {
    resetRunUI()
    document.getElementById("artifact-menu-btn").textContent = document.getElementById("btn-"+ctx).textContent
    switch (ctx) {
      case "main":
        appState.runMethod = runProgram
        break
      case "io":
        appState.runMethod = runIO
        setTerminalVisibility(true)
        break
      case "spec":
        appState.runMethod = runSpec
        document.getElementById("run-spec-controls").classList.remove("hidden")
        output.classList.add("h-[93%]")
        output.classList.remove("h-full")
        break
    }
    toggleArtifactMenu()
  }
  return f
}

function resetContext() {
  appState.runMethod = defaultRunMethod
  document.getElementById("artifact-menu-btn").textContent = "select artifact"
  resetRunUI()
}

function resetRunUI() {
  setOverflowStatus(false)
  setTerminalVisibility(false)
  output.textContent=""
  document.getElementById("run-spec-controls").classList.add("hidden")
  output.classList.remove("h-[93%]")
  output.classList.add("h-full")
}

const toggleArtifactMenu = toggleArtifactMenuInit()
function toggleArtifactMenuInit() {
  let status = false
  let f = () => {
    if (status) {
      document.getElementById("artifact-menu-list").classList.add("hidden")
    } else {
      document.getElementById("artifact-menu-list").classList.remove("hidden")
    }
    status = !status
  }
  return f
}

function toogleCollapse(elemPrefix) {
  return () => {
    let elemClassList = document.getElementById(elemPrefix+"-collapse").classList
    let statusElem = document.getElementById(elemPrefix+"-status")
    if (elemClassList.contains("hidden")) {
      elemClassList.remove("hidden")
      statusElem.textContent = "-"
    } else {
      elemClassList.add("hidden")
      statusElem.textContent = "+"
    }
  }
}

// setup buttons
function setupButton(buttonId,action) {
  document.getElementById(buttonId).addEventListener("click",action)
}

function setupButtons(xs) {
  xs.forEach((x) => setupButton(x.buttonId,x.action))
}

setupButtons(
  [ {buttonId: "btn-constraints", action: loadSetup(constraintSetup)}
  , {buttonId: "btn-constraints", action: (() => setupConstraints(true))}

  , {buttonId: "btn-random", action: loadSetup(randomSetup)}
  , {buttonId: "btn-random", action: (() => setupConstraints(false))}

  , {buttonId: "btn-empty", action: loadExample(emptyTemplate)}
  , {buttonId: "btn-sum", action: loadExample(sumExample)}
  , {buttonId: "btn-sum2", action: loadExample(sumExampleWithOutput)}
  , {buttonId: "btn-sumToZero", action: loadExample(sumToZero)}
  , {buttonId: "btn-overflow", action: loadExample(productExample)}
  , {buttonId: "btn-singlePath", action: loadExample(singlePath)}
  , {buttonId: "btn-fullTree", action: loadExample(fullTree)}
  , {buttonId: "btn-stringExample", action: loadExample(stringExample)}
  , {buttonId: "btn-untilValid", action: loadExample(untilValidExample)}
  , {buttonId: "btn-greeter", action: loadExample(greeter)}
  , {buttonId: "btn-palindrom", action: loadExample(palindrom)}
  , {buttonId: "compile-button", action: sendSrc}

  , {buttonId: "run-button", action: (() => appState.runMethod())}
  , {buttonId: "artifact-menu-btn", action: toggleArtifactMenu}
  , {buttonId: "btn-main", action: setRunContext("main")}
  , {buttonId: "btn-io", action: setRunContext("io")}
  , {buttonId: "btn-spec", action: setRunContext("spec")}

  , {buttonId: "btn-more", action: toggleMore}
  , {buttonId: "btn-smt-code", action: runSMTCode}
  , {buttonId: "btn-sample-input", action: runSampleInput}

  , {buttonId: "btn-problem-forwards", action: (() => changeProblem(1))}
  , {buttonId: "btn-problem-backwards", action: (() => changeProblem(-1))}

  , {buttonId: "no-loops-container", action: toogleCollapse("no-loops")}
  , {buttonId: "simple-loops-container", action: toogleCollapse("simple-loops")}
  , {buttonId: "complex-loops-container", action: toogleCollapse("complex-loops")}
  ]
)

function loadSetup(src) {
  let f = () => {
    let len = setupView.state.doc.length
    setupView.dispatch({changes: {from: 0, to:len, insert: src}})
  }
  return f
}

function loadExample(src) {
  let f = (event) => {
    let len = srcView.state.doc.length
    srcView.dispatch({changes: {from: 0, to:len, insert: src}})
    event.stopPropagation()
  }
  return f
}

function changeProblem(n) {
  if (appState.smtProblems != null) {
    let newIndex = appState.currentProblem + n
    if (0 <= newIndex && newIndex < appState.smtProblems.length) {
      appState.currentProblem = newIndex
      updateProblemDispaly()
    }
  }
}

function sendSrc() {
  if (!appState.busy) {
    if (appState.armed) {
      setArmed(false)
    }
    appState.busy = true
    output.textContent=""

    resetContext()
    setOverflowStatus(false)

    appState.ws = new WebSocket(websocketURL)

    let src = srcView.state.doc.toString();
    appState.ws.onopen = () => {
      appState.ws.send("send_src")
      if (appState.setupWithConstraint) {
        appState.ws.send("constraints")
        setSendConstraints(true)
      } else {
        appState.ws.send("random")
        setSendConstraints(false)
      }
      appState.ws.send(src)
      appState.ws.send("EOF")
    }

    appState.ws.onmessage = (msg) => {
      switch (msg.data) {
        case "INFO: success":
          setArmed(true)
          document.getElementById("output").textContent="Compiled successfully."
          appState.lastCompiledSrc=src
          updateRecompileInfo()
          break;
        case "INFO: failure":
          break;
        default:
          document.getElementById("output").textContent+=msg.data+"\n"
      }
      appState.busy = false
    }

    appState.ws.onclose = () => {
      setArmed(false,true)
      updateRecompileInfo()
      appState.busy = false
    }
  }
}

function runProgram() {
  if (appState.armed && !appState.busy) {
    appState.busy = true
    startToStop()

    // reset overflow status
    setOverflowStatus(false)
    setTerminalVisibility(false)

    output.textContent=""
    appState.ws.onmessage = msg => {
      let str = msg.data + "\n";

      switch (msg.data) {
        case "INFO: terminated":
          stopToStart()
          appState.busy = false
          return
        case "AsyncCancelled":
          document.getElementById("output").textContent+="Stopped"
          break;
        default:
          switch (true) {
            // overflow detected?
            case str.search(/Overflow of Int range detected/) >= 0:
              setOverflowStatus(true)
              return;
            // test if output should be cleared
            case str.search(/tests\)/) >= 0: // during testing
              output.textContent = "";
              break;
            case str.search(/generated/) >= 0: // finished constant based testing
              output.textContent = "";
              break;
            case str.search(/overflows/) >= 0 && output.textContent.search(/generated/) < 0: // finished random testing, all outcomes + overflows
              output.textContent = "";
              break;
            case str.search(/\+\+\+/) >= 0 && output.textContent.search(/generated/) < 0 && output.textContent.search(/overflows/) < 0: // finished random testing success (no overflows)
              output.textContent = "";
              break;
            case str.search(/\*\*\*/) >= 0 && output.textContent.search(/generated/) < 0 && output.textContent.search(/overflows/) < 0: // finished random testing failure/gave up <(no overflows)
              output.textContent = "";
              break;
            }
            output.textContent += str;
          }
        }
    appState.ws.send("run")
  }
}

function stopExecution() {
    appState.ws.send('~')
    appState.busy = false
    stopToStart()
}

function startToStop() {
  document.getElementById("run-button").classList.add("hidden")
  document.getElementById("stop-button").classList.remove("hidden")
  document.getElementById("stop-button").addEventListener("click",stopExecution,{once:true})
}

function stopToStart() {
  document.getElementById("stop-button").classList.add("hidden")
  document.getElementById("run-button").classList.remove("hidden")
  document.getElementById("stop-button").removeEventListener("click",stopExecution)
}

function runSMTCode() {
  if (appState.armed && !appState.busy) {
    appState.busy = true
    updateMoreView()
    document.getElementById("output-more").textContent = ""

    appState.currentProblem = 0
    appState.smtProblems = []

    let buffer = ""
    appState.ws.onmessage = msg => {
      switch (msg.data) {
        case "INFO: terminated":
            appState.busy = false
            stopToStart()
            switch (true) {
              case buffer != "": // Assumption: no paths found due to exception in specification
                document.getElementById("output-more").textContent = buffer
                break
              case document.getElementById("output-more").textContent == "":
                document.getElementById("output-more").textContent = "the specification has no paths of exactly the requested length"
                break
            }
            break;
        case "AsyncCancelled":
          document.getElementById("output-more").textContent+="Stopped"
          break;
        case "INFO: end of smt problem":
          appState.smtProblems.push(buffer)
          updateProblemDispaly()
          buffer = ""
          break;

        default:
          buffer += msg.data + "\n"
      }
    }
    appState.ws.send("smt_code")
    appState.ws.send(document.getElementById("inp-length").value)
    startToStop()
  }
}

function runSampleInput() {
  if (appState.armed && !appState.busy) {
    appState.busy = true
    updateMoreView()
    document.getElementById("output-more").textContent = ""
    appState.ws.onmessage = msg => {
      switch (msg.data) {
        case "INFO: terminated":
            appState.busy = false
            stopToStart()
          break;
        case "AsyncCancelled":
          document.getElementById("output-more").textContent+="Stopped"
          break;
        default:
          document.getElementById("output-more").textContent += msg.data + "\n"
      }
    }
    startToStop()
    appState.ws.send("sample_input")
    appState.ws.send(document.getElementById("inp-inputs").value)
    if (appState.sendConstraints) {
      appState.ws.send(document.getElementById("inp-length").value)
    }
  }
}

function runIO() {
  if (appState.armed && !appState.busy) {
    appState.busy = true

    if (appState.lastCompiledSrc.search(/\sgetChar\s/) >= 0) {
      alert("Warning: interactive behavior of getChar can be unreliable (might require <ENTER> to be sent)")
    }

    setTerminalVisibility(true)
    setOverflowStatus(false)
    term.clear()
    term.writeLine("program")

    appState.ws.onmessage = msg => {
      switch (msg.data) {
        case "INFO: terminated":
          appState.busy = false
          term.unsetCallback()
          term.prompt()
          stopToStart()
          break
        case "AsyncCancelled":
          term.writeLine("Stopped")
          break;
        default:
          switch (true) {
            case msg.data.startsWith("<char>"):
              term.writeLine(msg.data.slice(6),false)
              break
            case msg.data.startsWith("<str>"):
              term.writeLine(msg.data.slice(5),false)
              break
            case msg.data.startsWith("<line>"):
              term.writeLine(msg.data.slice(6),true)
              break
            default:
              term.writeLine(msg.data)
          }
      }
    }
    appState.ws.send("run_io")
    startToStop()

    term.onUserInputLine(line => {
      appState.ws.send(line)
    })
  }
}

function runSpec() {
  if (appState.armed && !appState.busy) {
    appState.busy = true

    output.textContent = ""
    setOverflowStatus(false)

    appState.ws.onmessage = msg => {
      switch (msg.data) {
        case "INFO: Overflow of Int range detected":
          setOverflowStatus(true)
          break;
        case "INFO: terminated":
          appState.busy = false
          stopToStart()
          break
        default:
          output.textContent += msg.data
      }
    }
    appState.ws.send("run_spec")
    let str = document.getElementById("input-seq").value
    appState.ws.send('['+str+']')
    startToStop()

  }
}
