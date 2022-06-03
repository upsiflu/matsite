/*---- Custom Element ----*/

customElements.define(
  "copy-text",
  class extends HTMLElement {
    constructor() {
      super();
    }
    setClipboard(id, button, text) {
      var node = document.querySelector("#" + id);
      node.contentEditable = "true";
      button.classList.add("success");
      setTimeout(() => button.classList.remove("success"), 500);
      console.log("node", node);
      node.innerText = text;
      console.log("node.innerText", node.innerText);
      if (window.getSelection) {
        var selection = window.getSelection();
        var range = document.createRange();
        range.selectNodeContents(node);
        selection.removeAllRanges();
        selection.addRange(range);
      }
      document.execCommand("copy");
    }
    connectedCallback() {
      // CLIPBOARD

      this.innerHTML = `<span id="mailTeam">movingAcrossThresholds@gmail.com</span>`;
      /*\
        <button onclick="setClipboard ('mailTeam', this, 'movingAcrossThresholds@gmail.com')">[copy]</button>;*/
    }
    static get observedAttributes() {
      return [];
    }
  }
);
