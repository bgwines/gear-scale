
createGearItem = function (form) {
  var name = document.getElementById("createGearItemForm_Name").value;
  var isPersonal = document.getElementById(
    "createGearItemForm_IsPersonal").value;
  var oz = document.getElementById("createGearItemForm_Oz").value;
  var kind = document.getElementById("createGearItemForm_Kind").value;

  const payload = {
    "name": name,
    "isPersonal": isPersonal,
    "oz": oz,
    "kind": kind};

  console.log("payload: " + payload);

  postPutGearItem(payload,
                  (r) => { console.log("onSuccess: " + r) },
                  (e) => { console.log("onError: " + e) }
                 );

  return false;
};
