import { Elm } from "./src/Main.elm";
import {} from "./append-log";
import {} from "./closest-aisle";
import {} from "./vimeo-video";
import {} from "./copy-text";

history.scrollRestoration = "manual";

var app = Elm.Main.init({
  node: document.getElementById("app"),
});
app.ports.pleaseCenter.subscribe(message => {
  // console.log("CENTERING");
  window.setTimeout(
    () =>
      requestAnimationFrame(() =>
        document
          .querySelector(".screenBackground")
          ?.scrollIntoView({ behavior: "smooth", block: "center", inline: "center" })
      ),
    250
  );
});
app.ports.pleaseConfirm.subscribe(function (message) {
  document.getElementById(message)?.classList.add("blink");
});
