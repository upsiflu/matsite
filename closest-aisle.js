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

        let pivotOf = rect => ({ x: rect.x + rect.width * 0.4, y: rect.y + rect.height * 0.4 });
        let isInside = (point, rect) =>
          point.x > rect.x && point.x < rect.x + rect.width && point.y > rect.y && point.y < rect.y + rect.height;
        let manhattanDistance = (p, q) => Math.abs(p.x - q.x) + Math.abs(p.y - q.y);
        let centerOf = rect => ({ x: rect.x + rect.width / 2, y: rect.y + rect.height / 2 });
        let difference = (point0, point1) => ({ x: point1.x - point0.x, y: point1.y - point0.y });
        let middleOffset = (rect0, rect1) => difference(centerOf(rect0), centerOf(rect1));
        let bottom = rect => rect.y + rect.height;
        let right = rect => rect.x + rect.width;
        let topLeft = rect => rect;
        let outOfScope = rect =>
          rect.x > window.innerWidth ||
          rect.y > window.innerHeight ||
          rect.x + rect.width < 0 ||
          rect.y + rect.height < 0;
        let topRight = rect => ({ x: rect.x + rect.width, y: rect.y });
        let bottomLeft = rect => ({ x: rect.x, y: rect.y + rect.height });
        let boundOffset = (to, from) => {
          if (to.x < from.x || to.y < from.y) {
            return difference(topLeft(from), topLeft(to));
          } else if (right(to) > right(from)) {
            return difference(topRight(from), topRight(to));
          } else if (bottom(to) > bottom(from)) {
            return difference(bottomLeft(from), bottomLeft(to));
          } else return { x: 0, y: 0 };
        };

        let focusRect = document.querySelector(".F>.bounds").getBoundingClientRect();
        let pivot = { x: window.innerWidth / 2, y: window.innerHeight / 2 };

        let focusPoint = pivotOf(focusRect);
        let minimumDistance = manhattanDistance(focusPoint, pivot);

        var closestAisleSegment = null;

        for (let a of document.querySelectorAll(".A>.bounds")) {
          let aisleSegmentRect = a.getBoundingClientRect();
          console.log("is inside?", pivot, aisleSegmentRect);
          if (isInside(pivot, aisleSegmentRect)) {
            console.log("YES!");

            let dist = manhattanDistance(pivotOf(aisleSegmentRect), pivot);

            if (dist < minimumDistance) {
              minimumDistance = dist;
              closestAisleSegment = aisleSegmentRect;
              closestAisleSegment.id = a.parentElement.id;
            }
          }
        }
        if (closestAisleSegment) {
          closestAisle.allowScroll = false;

          closestAisle.vector = boundOffset(closestAisleSegment, focusRect);

          console.log(closestAisleSegment.id, "has been scrolled close to the viewport pivot by", closestAisle.vector);
          closestAisle.dispatchEvent(
            new CustomEvent("scrolledToA", {
              detail: closestAisleSegment.id,
            })
          );
        } else {
          if (outOfScope(focusRect)) closestAisle.dispatchEvent(new CustomEvent("scrolledIntoNowhere"));
        }
        document.documentElement.classList.remove("is-scrolling");
      };
      window.addEventListener(
        "scroll",
        function () {
          if (closestAisle.timeout) window.clearTimeout(closestAisle.timeout);
          if (closestAisle.allowScroll) {
            closestAisle.timeout = window.setTimeout(dispatchClosestAisle, 250);
            document.documentElement.classList.add("is-scrolling");
          }
        },
        { passive: true }
      );
    }
    attributeChangedCallback(_name, _oldValue, _newValue) {
      console.log("attribute changed:", _name, ":", _oldValue, "->", _newValue);
      var closestAisle = this;
      if (closestAisle.timeout) window.clearTimeout(closestAisle.timeout);

      if (closestAisle.vector) {
        let root = document.documentElement;

        let previousOffset = {
          x: parseInt(getComputedStyle(root).getPropertyValue("--x-offset") || 0),
          y: parseInt(getComputedStyle(root).getPropertyValue("--y-offset") || 0),
        };

        let newOffset = {
          x: previousOffset.x + closestAisle.vector.x,
          y: previousOffset.y + closestAisle.vector.y,
        };

        console.log("move x/y by:", closestAisle.vector);
        console.log("--x-offset before", previousOffset.x);
        console.log("scrollLeft", closestAisle.parentElement?.scrollLeft);
        root.style.setProperty("--x-offset", `${newOffset.x}px`);
        root.style.setProperty("--y-offset", `${newOffset.y}px`);
        console.log("--x-offset after", parseInt(getComputedStyle(root).getPropertyValue("--x-offset")));

        /*
      
      if (closestAisle.vector) {
        closestAisle.parentElement.scrollBy({
          top: closestAisle.vector.x,
          left: closestAisle.vector.y,
          behavior: "instant",
        });
      }
      */
      }
      //closestAisle.scrollIntoView({ behavior: "auto", block: "start", inline: "start" });
      window.setTimeout(() => {
        closestAisle.allowScroll = true;
      }, 500);
    }
    static get observedAttributes() {
      return ["increment"];
    }
  }
);
