/*---- Custom Element ----*/

customElements.define(
  "vimeo-video",
  class extends HTMLElement {
    constructor() {
      super();
    }
    connectedCallback() {
      console.log("VIMEO INTRO PLAYER");
      /**/
      var vimeoVideo = this;

      var vVideo = "714390804?h=3f386f21b9";
      var hVideo = "714389952?h=c32a03d366";

      var clientWidth = document.querySelector("html").clientWidth;
      var clientHeight = document.querySelector("html").clientHeight;

      var lnk = clientHeight > clientWidth ? vVideo : hVideo;

      /**/
      var htmlt = `<iframe id="intro-player" background=True autoplay=True byline=False title=False dnt=True loop=True src="https://player.vimeo.com/video/${lnk}&amp;badge=0&amp;background=True&amp;byline=False&amp;dnt=True&amp;autoplay=True&amp;loop=True&amp;title=False&amp;autopause=0&amp;player_id=0&amp;app_id=58479" width="${clientWidth}" height="${clientHeight}" frameborder="0" allow="autoplay; fullscreen; picture-in-picture" allowfullscreen title=""></iframe>`;

      this.innerHTML = htmlt;
    }
    static get observedAttributes() {
      return [];
    }
  }
);
