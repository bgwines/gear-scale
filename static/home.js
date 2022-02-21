
createGearItem = function (form) {
  var name = document.getElementById("createGearItemForm_Name").value;
  var isPersonal = document.getElementById(
    "createGearItemForm_IsPersonal").checked;
  var oz = document.getElementById("createGearItemForm_Oz").value;
  var kind = document.getElementById("createGearItemForm_Kind").value;

  const payload = {
    "_gearitemId": "",
    "_gearitemName": name,
    "_gearitemIsPersonal": !!(isPersonal),
    "_gearitemOz": parseFloat(oz),
    "_gearitemKind": kind,
    "_gearitemCreatorUserId": "Brett"
  };

  console.log("payload: " + payload);

  postPutGearItem(payload,
                  (r) => { console.log("onSuccess: " + r) },
                  (e) => { console.log("onError: " + e) }
                 );

  return false;
};
