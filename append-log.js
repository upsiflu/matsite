// Import the functions you need from the SDKs you need

import { initializeApp } from "firebase/app";
import { getFirestore, enableIndexedDbPersistence, collection, addDoc, getDocs, doc, onSnapshot, updateDoc, arrayUnion, arrayRemove } from "firebase/firestore";



// https://firebase.google.com/docs/web/setup#available-libraries



// MaTsite Firebase configuration
const firebaseConfig = {
  apiKey: "AIzaSyB-zvE4f5AXV9A3ke9wl_zGat8cYiXw8O0",
  authDomain: "matsite-1245a.firebaseapp.com",
  projectId: "matsite-1245a",
  storageBucket: "matsite-1245a.appspot.com",
  messagingSenderId: "841976917923",
  appId: "1:841976917923:web:b1fa5a7101ce1ca7a3b416"
};


// Initialize Firebase
const app = initializeApp(firebaseConfig);


// Initialize Cloud Firestore and get a reference to the service
const db = getFirestore(app);


// Enable Offline Cache
enableIndexedDbPersistence(db)
  .catch((err) => {
      if (err.code == 'failed-precondition') {
          // Multiple tabs open, persistence can only be enabled
          // in one tab at a a time.
          // ...
      } else if (err.code == 'unimplemented') {
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
      born: 1815
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
    born: 1912
  });

  console.log("Document written with ID: ", docRef.id);
} catch (e) {
  console.error("Error adding document: ", e);
}

}

var query = async function () {
// Query the docs
const querySnapshot = await getDocs(collection(db, "users"));
querySnapshot.forEach((doc) => {
  console.log(`${doc.id} => ${doc.data()}`);
});
};

store(); query();





const log = doc(db, "log", "entries");


const receive = onSnapshot(doc(db, "cities", "SF"), (doc) => {
    console.log("Current data: ", doc.data());
});


const append = async function (data) {
  console.log("append", data)
  await updateDoc(log, {
    actions: arrayUnion(data)
  });

}





/*---- Custom Element ----*/

customElements.define(
  "append-log",
  class extends HTMLElement {
    constructor() {
      super();
    }
    connectedCallback() {
      /**/
    }
    attributeChangedCallback(name, oldValue, newValue) {
      /**/
      console.log (name, oldValue, "------------->", newValue);
      if (name=="backlog") {append (newValue);}
    }
    static get observedAttributes() {
      return ["backlog"];
    }
  }
);
