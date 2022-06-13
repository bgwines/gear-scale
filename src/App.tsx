import React, { useCallback, useEffect, useState } from 'react';

import './App.css';
import sra from './sun-ribbon-2.jpeg';
import * as BackendApi from './client';
import GearItemForm from './GearItemForm';
import GearItemsTable from './GearItemsTable';
import TripForm from './TripForm';
import { GearItem, Trip } from './client.d';

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
  const emptyTrips:Array<Trip> = [];
  const [trips, setTrips] = useState(emptyTrips);


  React.useEffect(() => {
    BackendApi.getSearchgearitems("").then((r) => {
      if (items.length === 0) {  // TODO
        console.log("setting gear items");
        setItems(r);
      }
    });
  });

  React.useEffect(() => {
    BackendApi.getSearchtrips("").then((r) => {
      if (trips.length === 0) {  // TODO
        console.log("setting trips");
        setTrips(r);
      }
    });
  });

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
    BackendApi.postDeletegearitem(itemId).then((r) => {
      console.log("deleteGearItem success: " + r);
      const newItems = items.filter(f);
      setItems(newItems);
    });
  };

  console.log("rendering");
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
      </header>
    </div>
  );
}
