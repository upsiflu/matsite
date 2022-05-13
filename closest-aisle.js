


import _ from "lodash"


/*---- Custom Element ----*/

customElements.define(
    "closest-aisle",
    class extends HTMLElement {
      constructor() {
        super();
      }
      connectedCallback() {
        /**/

        document.addEventListener("scroll", ((e) => {
            _.debounce(() => {
                let aisle = document.querySelectorAll(".A");
                let optimum = window.innerWidth/2;
                let f = document.querySelector(".F").getBoundingClientRect();
                let fx = f.x+f.width/2; 
                var minimum = Math.abs(optimum-fx);
                var newCenterId = null;
                for (let a of aisle) {
                    let rect = a.getBoundingClientRect();
                    let x = rect.x+rect.width/2
                    let ad = Math.abs(optimum-x);
                    if (ad<minimum) {minimum=ad; newCenterId=a.id;}
                }
                if (newCenterId) {
                    console.log(newCenterId);
                    this.dispatchEvent(new CustomEvent("scrolledToA", {
                        detail: newCenterId
                      }))
                }
            }, 580)()}));

      }
      static get observedAttributes() {
        return [];
      }
    }
  );