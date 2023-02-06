/*---- Custom Element ----
 * Centers itself smoothly into the viewport immediately
 */

customElements.define(
  "center-me",
  class extends HTMLElement {
    constructor() {
      super();
    }

    connectedCallback() {
      window.requestAnimationFrame(() => this.parentElement?.scrollIntoView({ behavior: "smooth", block: "center", inline: "center" }));
    }

    static get observedAttributes() {
      return ["increment"];
    }
  }
);
