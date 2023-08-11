export {initTerminal}
import {Terminal} from "xterm"

function initTerminal() {
  function emptyCallback(line) {
    if (line == "program") {
      term.clear()
      document.getElementById("run-button").click()
    }
  }

  let lineCallback = emptyCallback
  let term = new Terminal({
  altClickMovesCursor: false,
  theme: {
    background: "#f3f4f6",
    foreground: "black",
    cursor: "black",
    }
  })
  term.open(document.getElementById('console'))
  term.prompt = () => term.write("> ")
  term.prompt()
  term.attachCustomKeyEventHandler(ev => {
    switch (ev.type) {
      case "keypress":
        switch (true) {
          case /^[\w-"']$/.test(ev.key):
            return true
          case (ev.code === "Space"):
            return true
          case ev.key === "Enter":
            return true
          default:
            return false
        }
      case "keydown":
        return ev.key === "Backspace"
      default:
        return false
    }
  })

  let buffer = ""
  term.onKey( ev => {
    switch (ev.key) {
      case "\r":
        term.write("\r\n")
        term.prompt()
        lineCallback(buffer)
        buffer=""
        break;
      case "\x7F":
        term.write("\b \b")
        buffer = buffer.slice(0,-1)
        if (term.buffer.cursorX > 2) {
          term.write("\b \b");
        }
        break;
      default:
        term.write(ev.key)
        buffer += ev.key
    }
  })

  return {
    writeLine : (str, nl=true) => {
      if (nl) {
        term.writeln(str)
      } else {
        term.write(str)
      }
    },
    onUserInputLine : func => {
      term.prompt = () => term.write("")
      lineCallback = func
    },
    unsetCallback : () => {
      term.prompt = () => term.write("> ")
      lineCallback = emptyCallback
    },
    clear : () => term.clear(),
    prompt : () => term.prompt(),
  }
}
