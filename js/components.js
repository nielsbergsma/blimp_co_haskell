"use strict";

class Popover extends HTMLElement {
  constructor() {
    super();
  }

  static get observedAttributes() { 
    return ["for", "orientation"];
  }

  connectedCallback() {
    const orientation = this.attributes["orientation"].value;
    const forElementId = this.attributes["for"].value;

    const forElement = document.getElementById(forElementId);
    this.style.position = "absolute"
    if (orientation === "right") {
      this.style.left = `${forElement.offsetLeft + forElement.offsetWidth + 10}px`;
    }
    else {
      this.style.left = `calc(${forElement.offsetLeft}px - 10px)`;
    }
    this.style.top = `${forElement.offsetTop}px`;
  }

  attributeChangedCallback() { 
    if (this.attributes["orientation"] && this.attributes["for"]) {
      this.connectedCallback(); 
    }
  }
}

export function install() {
    window.customElements.define('x-popover', Popover);
}