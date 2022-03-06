import React, { useCallback, useEffect, useState } from 'react';

import './App.css';
import sra from './sun-ribbon-2.jpeg';
import * as BackendApi from './backend_api.js';
import GearItemForm from './GearItemForm';
import GearItemsTable from './GearItemsTable';
import { GearItem } from './types';

import Button from '@mui/material/Button';

const getCleanGearItem = (): GearItem => {
  return {
    itemId: "",
    name: "",
    isPersonal: true,
    oz: -1,
    kind: "Base",
    creatorUserId: "",
  };
};


// TODO: "Rebalance group gear" feature
// TODO: fire the network request prior to mounting
// TODO: delete item
export default function App(_: any) {
  const cleanGearItem = getCleanGearItem();

  const emptyItems:Array<GearItem> = [];
  const [items, setItems] = useState(emptyItems);
  const [gearItemFormState, setGearItemFormState] = useState({
    isOpen: false,
    gearItem: cleanGearItem
  });


  React.useEffect(() => {
    BackendApi.getSearchGearItems("", (r: any) => {
      setItems(r);
    }, (e: any) => {
      console.log("error " + e);
    });
  }, []);

  const plusFunction = useCallback((event) => {
    if (event.keyCode === 187) {
      setGearItemFormState({
        isOpen: true,
        gearItem: cleanGearItem,
      });
    }
  }, []);

  useEffect(() => {
    document.addEventListener("keydown", plusFunction);
    return () => { document.removeEventListener("keydown", plusFunction); };
  }, [plusFunction]);

  const editItem = (itemId: string) => {
    const f = (i: GearItem): boolean => {
      return i.itemId === itemId;
    };

    const gearItem = items.find(f) as GearItem;
    setGearItemFormState({
      isOpen: true,
      gearItem: gearItem,
    });
  };

  // TODO: [+]
  return (
    <div className="App">
      <header className="App-header">
        <img src={sra}
          className="Sun-Ribbon"
          width="1000px"
          alt="Sun Ribbon Arete" />
       <br/>
        <div>
          <Button variant="outlined" onClick={() => setGearItemFormState({
              isOpen: true,
              gearItem: cleanGearItem})}>
            Add gear item [+]
          </Button>
          <GearItemForm
            isOpen={gearItemFormState.isOpen}
            onClose={() => setGearItemFormState({
              isOpen: false,
              gearItem: cleanGearItem})}
            gearItem={gearItemFormState.gearItem}
            editFormState={setGearItemFormState}
            isNewItem={gearItemFormState.gearItem.itemId === ""}
          />
        </div>
       <p>
          Items:
        </p>
      <GearItemsTable gearItems={items} editItem={editItem}/>
      </header>
    </div>
  );
}
