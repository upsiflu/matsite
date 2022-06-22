import _ from "lodash";

/*---- Custom Element ----
 *
 * Attributes: [increment]
 * Events: "scrolledToA"
 *          details: { id : .A segment closer than .F and closest to viewport-center
 *                   }
 * On each increment, scroll hard to the screen pivot
 */

customElements.define(
  "closest-aisle",
  class extends HTMLElement {
    constructor() {
      super();
    }
    connectedCallback() {
      var closestAisle = this;
      this.innerHTML = `<div id="pivot"> </div>`;
      document.addEventListener(
        "scroll",
        _.debounce(() => {
          /**
           * The `#pivot` is a fixed point on the viewport (sort of a cursor).
           * 1. Calculate the manhattan distance from the the pivot to the .F (focus segment) center
           * 2. For all .A (aisle segments), measure the manhattan distance to the screen center
           *    - find the smallest distance, if it's smaller than the benchmark
           *    - and then dispatch the id and vector to the scrolledTo segment
           */

          let pivotOf = rect => {
            return { x: rect.x + 24, y: rect.y + rect.height * 0.4 };
          };
          let manhattanDistance = (p, q) => Math.abs(p.x - q.x) + Math.abs(p.y - q.y);

          let pivot = document.querySelector("#pivot").getBoundingClientRect();

          let focusPoint = pivotOf(document.querySelector(".F").getBoundingClientRect());
          let minimumDistance = manhattanDistance(focusPoint, pivot);

          var closestAisleSegment = null;

          for (let a of document.querySelectorAll(".A")) {
            let aisleSegmentPoint = pivotOf(a.getBoundingClientRect());
            let dist = manhattanDistance(aisleSegmentPoint, pivot);

            if (dist < minimumDistance) {
              minimumDistance = dist;
              closestAisleSegment = { id: a.id, ...aisleSegmentPoint };
            }
          }
          if (closestAisleSegment) {
            console.log(closestAisleSegment.id, "has been scrolled close to the viewport pivot");
            console.log("viewport to pivot:", pivot);
            console.log("viewport to focus-pivot:", focusPoint);
            console.log("viewport to closestAisleSegment-pivot:", closestAisleSegment);

            closestAisle.style.marginLeft = `-${closestAisleSegment.x}px`;
            closestAisle.style.marginTop = `-${closestAisleSegment.y}px`;
            closestAisle.dispatchEvent(
              new CustomEvent("scrolledToA", {
                detail: closestAisleSegment.id,
              })
            );
          }
        }, 2000)(),
        { passive: true }
      );
    }
    attributeChangedCallback(_name, _oldValue, _newValue) {
      console.log("attribute changed:", _name, ":", _oldValue, "->", _newValue);
      var closestAisle = this;
      setTimeout(function () {
        closestAisle.scrollIntoView({ behavior: "auto", block: "center", inline: "center" });
      }, 1000);
    }
    static get observedAttributes() {
      return ["increment"];
    }
  }
);
