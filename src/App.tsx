import React, { useCallback, useEffect, useState } from 'react';

import './App.css';
import sra from './sun-ribbon-2.jpeg';
import * as BackendApi from './backend_api.js';
import GearItemForm from './GearItemForm';
import GearItemsTable from './GearItemsTable';
import { GearItem, GearKind } from './types';

import Button from '@mui/material/Button';

// TODO: "Rebalance group gear" button
// TODO: fire the network request prior to mounting
// TODO: delete item
export default function App(_: any) {
  const [items, setItems] = useState([]);
  const [gearItemFormStatus, setGearItemFormStatus] = useState({
    isOpen: false,
    itemForEditing: undefined
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
      setGearItemFormStatus({
        isOpen: true,
        itemForEditing: undefined,
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

    const itemForEditing = items.find(f);
    setGearItemFormStatus({
      isOpen: true,
      itemForEditing: itemForEditing,
    });
  };

  // TODO: [+]
  console.log("gearItemFormStatus.itemForEditing:" + gearItemFormStatus.itemForEditing);
  console.log("gearItemFormStatus.isOpen:" + gearItemFormStatus.isOpen);
  return (
    <div className="App">
      <header className="App-header">
        <img src={sra}
          className="Sun-Ribbon"
          width="1000px"
          alt="Sun Ribbon Arete" />
       <br/>
        <div>
          <Button variant="outlined" onClick={() => setGearItemFormStatus({
              isOpen: true,
              itemForEditing: undefined})}>
            Add gear item [+]
          </Button>
          <GearItemForm
            isOpen={gearItemFormStatus.isOpen}
            onClose={() => setGearItemFormStatus({
              isOpen: false,
              itemForEditing: undefined})}
            initialState={gearItemFormStatus.itemForEditing}
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
