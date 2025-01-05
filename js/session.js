function restoreSession() {
  signInDemo()
}

function signInDemo() {
  const session = { name: "J. Blimp", photoUrl: null, token: "" };
  const event = new CustomEvent("signedin", { detail: JSON.stringify(session) });
  window.dispatchEvent(event);
}

function signOut() {
  const event = new CustomEvent("signedout", { detail: JSON.stringify({}) });
  window.dispatchEvent(event);
}

export {
  restoreSession, 
  signInDemo,
  signOut
}