/*---- Custom Element ----*/

customElements.define(
  "vimeo-video",
  class extends HTMLElement {
    constructor() {
      super();
    }
    connectedCallback() {
      /**/
      var vimeoVideo = this;

      var vVideo = "714390804?h=3f386f21b9";
      var hVideo = "714389952?h=c32a03d366";

      var clientWidth = document.querySelector("html").clientWidth;
      var clientHeight = document.querySelector("html").clientHeight;

      var lnk = clientHeight > clientWidth ? vVideo : hVideo;

      /**/
      var htmlt = `<iframe id="intro-player" autoplay=1 loop=1 background=1 autoplay=1 byline=False title=False dnt=True loop=1 src="https://player.vimeo.com/video/${lnk}&amp;badge=0&amp;background=True&amp;byline=False&amp;dnt=True&amp;autoplay=True&amp;loop=True&amp;title=False&amp;autopause=0&amp;player_id=0&amp;app_id=58479" width="${clientWidth}" height="${clientHeight}" frameborder="0" allow="autoplay; fullscreen; picture-in-picture" allowfullscreen title=""></iframe>`;

      var htmlt2 = `<iframe  id="vi-banner-video"
       src="https://player.vimeo.com/video/${lnk}&autoplay=1&portrait=0&loop=1&color=70f0a0&title=0&byline=0&dnt=1" 
        width="${clientWidth}" height="${clientHeight}" frameborder="0" 
        allow="autoplay; fullscreen; picture-in-picture" allowfullscreen></iframe>`;

      this.innerHTML = htmlt2;
    }
    static get observedAttributes() {
      return [];
    }
  }
);
