import _ from "lodash";

/*---- Custom Element ----
 * This is a scroll cursor,
 * - dispatching the closest aisle id when scrolled over it
 * - adjusting its own margins to match the
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
      closestAisle.allowScroll = true;
      var pivot = document.createElement("div");
      pivot.id = "pivot";
      this.parentNode.appendChild(pivot);
      const dispatchClosestAisle = () => {
        /**
         * The `#pivot` is a fixed point on the viewport (sort of a cursor).
         * 1. Calculate the manhattan distance from the the pivot to the .F (focus segment) center
         * 2. For all .A (aisle segments), measure the manhattan distance to the screen center
         *    - find the smallest distance, if it's smaller than the benchmark
         *    - and then dispatch the id and vector to the scrolledTo segment
         * 3. Set own margins to match the x and y position of a.pivot onscreen
         * 4. App is responsible to scroll such that the real screen matches this element's bounding box
         */

        console.log("scrolling has ended");

        let pivotOf = rect => ({ x: rect.x + 24, y: rect.y + rect.height * 0.4 });
        let isInside = (point, rect) =>
          point.x > rect.x && point.x < rect.x + rect.width && point.y > rect.y && point.y < rect.y + rect.height;
        let manhattanDistance = (p, q) => Math.abs(p.x - q.x) + Math.abs(p.y - q.y);

        let pivot = document.querySelector("#pivot").getBoundingClientRect();

        let focusPoint = pivotOf(document.querySelector(".F").getBoundingClientRect());
        let minimumDistance = manhattanDistance(focusPoint, pivot);

        var closestAisleSegment = null;

        for (let a of document.querySelectorAll(".A")) {
          let aisleSegmentRect = a.getBoundingClientRect();
          if (isInside(pivot, aisleSegmentRect)) {
            let dist = manhattanDistance(pivotOf(aisleSegmentRect), pivot);

            if (dist < minimumDistance) {
              minimumDistance = dist;
              console.log("aisleSegmentRect", aisleSegmentRect);
              closestAisleSegment = { x: aisleSegmentRect.x, y: aisleSegmentRect.y, id: a.id };
              console.log("closestAisleSegment", closestAisleSegment);
            }
          }
        }
        if (closestAisleSegment) {
          closestAisle.allowScroll = false;
          console.log(closestAisleSegment.id, "has been scrolled close to the viewport pivot");

          closestAisle.vector = { x: pivot.x - closestAisleSegment.x, y: pivot.y - closestAisleSegment.y };

          closestAisle.style.transform = `translate(${closestAisle.vector.x}px, ${closestAisle.vector.y}px)`;

          console.log("PIVOT", pivot);
          console.log("closestAisleSegment", closestAisleSegment);
          console.log("VECTOR", closestAisle.vector);

          closestAisle.classList.add("navigatingByScroll");

          closestAisle.dispatchEvent(
            new CustomEvent("scrolledToA", {
              detail: closestAisleSegment.id,
            })
          );
        }
      };
      window.addEventListener(
        "scroll",
        function () {
          if (closestAisle.timeout) window.clearTimeout(closestAisle.timeout);
          if (closestAisle.allowScroll) {
            closestAisle.timeout = window.setTimeout(dispatchClosestAisle, 80);
          }
        },
        { passive: true }
      );
    }
    attributeChangedCallback(_name, _oldValue, _newValue) {
      console.log("attribute changed:", _name, ":", _oldValue, "->", _newValue);
      var closestAisle = this;
      if (closestAisle.timeout) window.clearTimeout(closestAisle.timeout);

      console.log("now scroll into view:", closestAisle);
      console.log("scroll parent by:", closestAisle.vector);
      /*
      
      if (closestAisle.vector) {
        closestAisle.parentElement.scrollBy({
          top: closestAisle.vector.x,
          left: closestAisle.vector.y,
          behavior: "instant",
        });
      }
      */
      //closestAisle.scrollIntoView({ behavior: "auto", block: "start", inline: "start" });
      window.requestAnimationFrame(() => closestAisle.classList.remove("navigatingByScroll"));
      window.requestAnimationFrame(() => {
        closestAisle.scrollIntoView({ behavior: "auto", block: "start", inline: "start" });
        window.requestAnimationFrame(() => {
          closestAisle.allowScroll = true;
        });
      });
    }
    static get observedAttributes() {
      return ["increment"];
    }
  }
);
