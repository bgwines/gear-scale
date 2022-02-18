
import * as api from "./api.js";

api.getPoint((r) => {
  console.log(r);
}, (e) => {
  console.log(e)
});

api.getBooks("haskell", (r) => {
  console.log(r);
}, (e) => {
  console.log(e)
});
