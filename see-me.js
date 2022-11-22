import _ from "lodash";

/*---- Custom Element ----
 * This is a scroll cursor,
 * dispatching the closest aisle id when scrolled over it
 * Attributes: [increment]
 * Events: "scrolledToA"
 *          details: { id : .A segment closer than .F and closest to viewport-center
 *                   }
 * On each increment, scroll hard to the screen pivot
 */

customElements.define(
  "see-me",
  class extends HTMLElement {
    constructor() {
      super();
    }
    connectedCallback() {
    }
    attributeChangedCallback(_name, _oldValue, _newValue) {

      //closestAisle.scrollIntoView({ behavior: "auto", block: "start", inline: "start" });
      window.setTimeout(() => {
        this.scrollIntoView({
          behavior: 'smooth'
        });
      }, 1000);
    }
    static get observedAttributes() {
      return ["increment"];
    }
  }
);
