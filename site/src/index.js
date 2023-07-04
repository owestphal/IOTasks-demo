let ghcVersion = document.getElementById("ghc-version");

ghcVersion.innerHTML = "???";
getGhcVersion()

function getGhcVersion() {
  let ws = new WebSocket("ws://localhost:8080/")
  ws.onopen = () => {
    ws.send("get_info")
    ws.send("ghc_version")
  }

  ws.onmessage = evt => {
    ghcVersion.innerHTML = evt.data;
  }
}
