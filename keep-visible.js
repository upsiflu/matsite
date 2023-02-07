/*---- Custom Element ----
 * Centers itself smoothly into the viewport immediately
 */

customElements.define(
  "keep-visible",
  class extends HTMLElement {
    constructor() {
      super();
    }

    connectedCallback() {
      let options = {
        rootMargin: "-10% -10% -10% -10%",
        threshold: 0
      }

      let intersected = (entries, observer) => {
        entries.forEach((entry) => {
          if (!entry.isIntersecting) {
            var rect = this.parentElement?.getBoundingClientRect();

            console.log(rect, this.parentElement?.id)


            let root = document.documentElement;
            root.style.setProperty("--x-offset", `0px`);
            root.style.setProperty("--y-offset", `0px`);

            window.requestAnimationFrame(() => this.parentElement?.querySelector(".anchor")?.scrollIntoView(
              { behavior: "smooth", block: "center", inline: "center" }));

          }
        });
        setTimeout(() => {
          observer.disconnect();
          observer = new IntersectionObserver(intersected, options);
          if (this.parentElement?.id)
            observer.observe(this.parentElement || this);
        }, 5000)
      }


      let observer;
      setTimeout(() => {
        observer = new IntersectionObserver(intersected, options);
        if (this.parentElement?.id)
          observer.observe(this.parentElement || this);
      }, 500)
    }




    static get observedAttributes() {
      return ["increment"];
    }
  }
);
