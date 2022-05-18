// Import the functions you need from the SDKs you need

import { initializeApp } from "firebase/app";
import {
  getFirestore,
  enableIndexedDbPersistence,
  collection,
  addDoc,
  getDocs,
  getDoc,
  doc,
  onSnapshot,
  updateDoc,
  arrayUnion,
  arrayRemove,
} from "firebase/firestore";
import _ from "lodash";

import Squire from "./squire";

// https://firebase.google.com/docs/web/setup#available-libraries

// MaTsite Firebase configuration
const firebaseConfig = {
  apiKey: "AIzaSyB-zvE4f5AXV9A3ke9wl_zGat8cYiXw8O0",
  authDomain: "matsite-1245a.firebaseapp.com",
  projectId: "matsite-1245a",
  storageBucket: "matsite-1245a.appspot.com",
  messagingSenderId: "841976917923",
  appId: "1:841976917923:web:b1fa5a7101ce1ca7a3b416",
};

// Initialize Firebase
const app = initializeApp(firebaseConfig);

// Initialize Cloud Firestore and get a reference to the service
const db = getFirestore(app);

// Enable Offline Cache
enableIndexedDbPersistence(db).catch(err => {
  if (err.code == "failed-precondition") {
    console.log(
      "Edits are not persisted while you are offline. The most probablie reason is that you have this site open in multiple tabs."
    );
    // Multiple tabs open, persistence can only be enabled
    // in one tab at a a time.
    // ...
  } else if (err.code == "unimplemented") {
    console.log(
      "Edits are not persisted while you are offline. Use a different browser to enable this feature or make sure to remain online."
    );
    // The current browser does not support all of the
    // features required to enable persistence
    // ...
  }
});

var store = async function () {
  // Add a Doc
  try {
    const docRef = await addDoc(collection(db, "users"), {
      first: "Ada",
      last: "Lovelace",
      born: 1815,
    });
    console.log("Document written with ID: ", docRef.id);
  } catch (e) {
    console.error("Error adding document: ", e);
  }

  // Add second Doc
  try {
    const docRef = await addDoc(collection(db, "users"), {
      first: "Alan",
      middle: "Mathison",
      last: "Turing",
      born: 1912,
    });

    console.log("Document written with ID: ", docRef.id);
  } catch (e) {
    console.error("Error adding document: ", e);
  }
};

var query = async function () {
  // Query the docs
  const querySnapshot = await getDocs(collection(db, "users"));
  querySnapshot.forEach(doc => {
    console.log(`${doc.id} => ${doc.data()}`);
  });
};

/*---- Custom Elements ----*/

/*---- Append Action to Log ----*/

const log = doc(db, "log", "entries");

const append = async function (data) {
  console.log("append");
  try {
    await updateDoc(log, { actions: arrayUnion(data) });
  } catch (error) {
    console.log("error in append", error);
  }
};

const overwrite = async function (data) {
  console.log("overwrite");
  try {
    await updateDoc(log, { actions: data });
  } catch (error) {
    console.log("error in overwrite", error);
  }
};

customElements.define(
  "append-log",
  class extends HTMLElement {
    constructor() {
      super();
    }
    connectedCallback() {
      var appendLog = this;
      /**/
      const unsub = onSnapshot(
        log,
        doc => {
          requestAnimationFrame(() => {
            console.log("onSnapshot");
            appendLog.dispatchEvent(
              new CustomEvent("logReceived", {
                detail: doc.data().actions,
              })
            );
          });
        },
        error => console.log("SNAPSHOT ERROR:", error)
      );
    }
    disconnectedCallback() {
      // unsub();
    }
    attributeChangedCallback(name, oldValue, newValue) {
      /**/
      console.log("attribute changed:", name);
      if (name == "backlog") {
        append(JSON.parse(newValue));
      } else if (name == "overwrite") {
        overwrite(JSON.parse(newValue));
      }
    }
    static get observedAttributes() {
      return ["backlog", "overwrite"];
    }
  }
);

/*---- Custom Elements ----*/

/*---- Display and Edit Remote Html ----*/

const hypertext = doc(db, "hypertext", "entries");

const save = async function (id, content) {
  console.log("save", id);
  try {
    await updateDoc(hypertext, id, content);
  } catch (e) {
    console.error("Error saving", id, e);
  }
};

// var load = async function (id) {

//   if (docSnap.exists()) {
//     console.log("Document data:", docSnap.());
//   } else {
//     // doc.data() will be undefined in this case
//     console.log("No such document!");
//   }

//   // Query the docs
//   const querySnapshot = await getDocs(collection(db, "users"));
//   querySnapshot.forEach(doc => {
//     console.log(`${doc.id} => ${doc.data()}`);
//   });
// };

customElements.define(
  "sync-hypertext",
  class extends HTMLElement {
    constructor() {
      super();

      this.editor = this;
      this.field = document.createElement("article");
      this.replacement = document.createElement("article");
      this.squire = new Squire(this.field, {
        blockTag: "P",
      });
      this.squire.addEventListener("input", e => {
        _.throttle(function (t) {
          save(t.getAttribute("data-id"), t.squire.getHTML());
        }, 1000)(this);
      });
    }
    connectedCallback() {
      var editor = this.editor;
      onSnapshot(
        hypertext,
        doc => {
          requestAnimationFrame(() => {
            var content = doc.get(editor.getAttribute("data-id"));
            console.log("ON SNAPSHOT -- syncHypertext", doc, editor.getAttribute("data-id"));
            this.reflectState(content);
          });
        },
        error => console.log("SNAPSHOT ERROR in Hypertext:", error)
      );
      getDoc(hypertext)
        .then(docSnap => {
          if (docSnap.exists()) {
            console.log("Document data:", docSnap.get(editor.getAttribute("data-id")));
            this.initialize(docSnap.get(editor.getAttribute("data-id")));
          } else {
            // doc.data() will be undefined in this case
            console.log("No such document!");
          }
        })
        .catch(e => console.log(e));
    }
    disconnectedCallback() {}
    attributeChangedCallback(name, oldValue, newValue) {}
    static get observedAttributes() {
      return ["data-id", "state"];
    }
    reflectState(content) {
      if (this.hasAttribute("state") && this.getAttribute("state") == "editing") {
        if (this.contains(this.replacement)) this.removeChild(this.replacement);
        if (!this.contains(this.field)) this.appendChild(this.field);
        // this.squire.setHTML(content);
      } else {
        if (this.contains(this.field)) this.removeChild(this.field);
        if (this.replacement && !this.contains(this.replacement)) {
          this.appendChild(this.replacement);
        }
        this.replacement.innerHTML = content;
      }
    }
    initialize(content) {
      if (this.hasAttribute("state") && this.getAttribute("state") == "editing") {
        if (this.contains(this.replacement)) this.removeChild(this.replacement);
        if (!this.contains(this.field)) this.appendChild(this.field);
        this.squire.setHTML(content);
      }
    }
  }
);
