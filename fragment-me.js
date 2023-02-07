/*---- Custom Element ----
 * Parent receives the focus when intersecting with a 2%*1% rectangle on the viewport
 * at 40% 49% 59% 49% from top, right, bottom, left respectively.
 * 
 * custom-element fragment-me
 * 
 * Knows its parent as well as its id
 * 
 * WHEN parent is below the pivot-point at 40vh, 50vw from top left
   AND there has been no scrolling in the previous animationFrame
   THEN replace the hash by id
 * 
 * In response, Restrictive will receive a new hash
 * ...and can insert a center-me custom-element
 */

customElements.define(
  "fragment-me",
  class extends HTMLElement {


    constructor() {
      super();
    }
    connectedCallback() {
      console.log("connectedCallback", this);

      var isHot = false

      let options = {
        rootMargin: "-40% -50% -59% -50%",
        threshold: 0
      }

      isHot;
      cursorSpeed;

      let intersected = (entries, observer) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting) {
            isHot = true
            var previousScroll = { x: window.scrollX, y: window.scrollY }
            let recurse = e => {
              const newScroll = { x: window.scrollX, y: window.scrollY };
              if (newScroll.x == previousScroll.x && newScroll.y == previousScroll.y && this.parentElement?.id) {
                console.log("SELF OBSERVED ", this.parentElement.id)

                let bottom = rect => rect.y + rect.height;
                let right = rect => rect.x + rect.width;
                let topLeft = rect => rect;
                let topRight = rect => ({ x: rect.x + rect.width, y: rect.y });
                let bottomLeft = rect => ({ x: rect.x, y: rect.y + rect.height });
                let difference = (point0, point1) => ({ x: point1.x - point0.x, y: point1.y - point0.y });

                let boundOffset = (to, from) => {
                  if (to.x < from.x || to.y < from.y) {
                    return difference(topLeft(from), topLeft(to));
                  } else if (right(to) > right(from)) {
                    return difference(topRight(from), topRight(to));
                  } else if (bottom(to) > bottom(from)) {
                    return difference(bottomLeft(from), bottomLeft(to));
                  } else return { x: 0, y: 0 };
                };

                let focusRect = document.querySelector(".F").getBoundingClientRect();
                let aisleSegmentRect = this.parentElement.getBoundingClientRect();

                let vector = boundOffset(aisleSegmentRect, focusRect);

                let root = document.documentElement;





                let distanceFromFtoA =
                  difference(focusRect, aisleSegmentRect);


                let previousOffset = {
                  x: parseInt(getComputedStyle(root).getPropertyValue("--x-offset") || 0),
                  y: parseInt(getComputedStyle(root).getPropertyValue("--y-offset") || 0),
                };

                let newOffset = {
                  x: previousOffset.x + distanceFromFtoA.x,
                  y: previousOffset.y + distanceFromFtoA.y,
                };


                const x = window.scrollX;
                const y = window.scrollY;


                var url_ob = new URL(document.URL);
                url_ob.hash = '#' + this.parentElement.id
                var new_url = url_ob.href;
                document.location.href = new_url;
                root.style.setProperty("--x-offset", `${newOffset.x}px`);
                root.style.setProperty("--y-offset", `${newOffset.y}px`);


                // Scroll to the previous location
                window.scrollTo(x, y);
              } else if (isHot) {
                previousScroll = newScroll;
                setTimeout(() => window.requestAnimationFrame(recurse), 5);
              }
            }
            setTimeout(() => window.requestAnimationFrame(recurse), 5);
          } else {
            isHot = false
          }
        });
      };
      let observer = new IntersectionObserver(intersected, options);
      if (this.parentElement?.id)
        observer.observe(this.parentElement?.querySelector(".bounds") || this);
    }

    attributeChangedCallback(_name, _oldValue, _newValue) {
      console.log("attributeChangedCallback", _name, _oldValue, _newValue);
    }


    static get observedAttributes() {
      return ["increment"];
    }
  }
);
