
import * as api from "api";

api.getPoint((r) => {
  console.log(r);
}, (e) => {
  console.log(e)
});
