import { Elm } from "./src/Main.elm";
import {} from "./append-log";
import {} from "./closest-aisle";

var app = Elm.Main.init({
  node: document.getElementById("app"),
});
setTimeout(center, 1);
app.ports.pleaseCenter.subscribe(function (message) {
  //document.getElementById(message)?.scrollIntoView({ behavior: "smooth", block: "center", inline: "center" });
  // document.getElementsByClassName("Center")[0]
  // 	?.scrollIntoView({ behavior: "smooth", block: "center", inline: "center" });
  // setTimeout(focus, 0);
  setTimeout(screen, 15);
});
app.ports.pleaseConfirm.subscribe(function (message) {
  document.getElementById(message)?.classList.add("blink");
});

const findMiddle = str => {
  console.log(str);
};
