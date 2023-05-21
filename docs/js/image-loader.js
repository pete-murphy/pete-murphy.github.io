(function () {
  class ImageLoader extends HTMLElement {
    constructor() {
      super();
      this.attachShadow({ mode: "open" });
    }

    static get observedAttributes() {
      return ["placeholder", "full-image"];
    }

    connectedCallback() {
      const placeholderPath = this.getAttribute("placeholder");
      const fullImagePath = this.getAttribute("full-image");

      const imageContainer = document.createElement("figure");
      imageContainer.style.width = "100%";
      imageContainer.style.margin = "0";

      const placeholderImg = document.createElement("img");
      placeholderImg.src = placeholderPath;
      placeholderImg.style.width = "100%";

      const fullImageImg = document.createElement("img");
      fullImageImg.src = fullImagePath;
      placeholderImg.style.width = "100%";
      fullImageImg.addEventListener("load", () => {
        placeholderImg.style.display = "none";
        fullImageImg.style.display = "block";
      });

      fullImageImg.style.display = "none";
      imageContainer.appendChild(placeholderImg);
      imageContainer.appendChild(fullImageImg);

      this.shadowRoot.appendChild(imageContainer);
    }

    attributeChangedCallback() {}
  }

  customElements.define("image-loader", ImageLoader);
})();
