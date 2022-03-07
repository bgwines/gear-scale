import React, { useCallback, useEffect, useState } from 'react';

import './App.css';
import sra from './sun-ribbon-2.jpeg';
import * as BackendApi from './backend_api.js';
import GearItemForm from './GearItemForm';
import GearItemsTable from './GearItemsTable';
import TripForm from './TripForm';
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
  const [tripFormIsOpen, setTripFormIsOpen] = useState(false);
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

  const handleKeyDown = useCallback((event) => {
    if (event.keyCode === 187 && !tripFormIsOpen) {
      // +
      setGearItemFormState({
        isOpen: true,
        gearItem: cleanGearItem,
      });
    }
  }, []);

  useEffect(() => {
    document.addEventListener("keydown", handleKeyDown);
    return () => { document.removeEventListener("keydown", handleKeyDown); };
  }, [handleKeyDown]);

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

  const deleteItem = (itemId: string) => {
    const f = (i: GearItem): boolean => {
      return i.itemId !== itemId;
    };

    // TODO: error dialogs
    BackendApi.postDeleteGearItem(itemId, (r: any) => {
      console.log("deleteGearItem success: " + r);
      const newItems = items.filter(f);
      setItems(newItems);
    }, (e: any) => {
      console.log("deleteGearItem error: " + e);
    });

  };

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
          <Button variant="outlined" onClick={() => setTripFormIsOpen(true)}>
            New trip
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
          <TripForm
            isOpen={tripFormIsOpen}
            isNewTrip={true}
            onClose={() => setTripFormIsOpen(false)}
          />
        </div>
        <div>
        <p>
           Items:
        </p>
        <GearItemsTable
          gearItems={items}
          editItem={editItem}
          deleteItem={deleteItem}/>
        </div>
      </header>  // TODO: move this
    </div>
  );
}
