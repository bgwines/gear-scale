
createGearItem = function (form: HTMLElement) {
  var name = document.getElementById("createGearItemForm_Name").value;
  var isPersonal = document.getElementById(
    "createGearItemForm_IsPersonal").checked;
  var oz = document.getElementById("createGearItemForm_Oz").value;
  var kind = document.getElementById("createGearItemForm_Kind").value;

  const payload: GearItem = {
    "itemId": "",
    "name": name,
    "isPersonal": !!(isPersonal),
    "oz": parseFloat(oz),
    "kind": kind,
    "creatorUserId": "Brett"
  };

  console.log("payload: " + payload);

  postPutGearItem(payload,
                  (r) => { console.log("onSuccess: " + r) },
                  (e) => { console.log("onError: " + e) }
                 );

  return false;
};
