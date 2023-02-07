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
      setTimeout(() => {
        window.requestAnimationFrame(() => this.parentElement?.querySelector(".anchor")?.scrollIntoView(
          { behavior: "smooth", block: "center", inline: "center" }));
      }, 100)
    }

    static get observedAttributes() {
      return ["increment"];
    }
  }
);
