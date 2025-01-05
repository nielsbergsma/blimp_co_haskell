"use strict";

function dispatchRouteChange(path) {
  const event = new CustomEvent("routechanged", { detail: path });
  window.dispatchEvent(event);
}

function handleClick(event) {
  const href = event.target && event.target.getAttribute("href");
    if (href && href.startsWith("/")) {
      history.pushState({}, null, href);
      dispatchRouteChange(href);
      event.preventDefault();
      return true;
    }
}

export function initialise() {
  addEventListener("popstate", function() {
    dispatchRouteChange(window.location.pathname);
  });
  
  addEventListener("click", handleClick); 
}