import React, { useCallback, useEffect, useState } from 'react';

import './App.css';
import sra from './sun-ribbon-2.jpeg';
import * as BackendApi from './backend_api.js';
import AddGearItemsForm from './AddGearItemsForm';
import GearItemsTable from './GearItemsTable';
//import { GearItem, GearKind } from './types';

import Button from '@mui/material/Button';

// TODO: "Rebalance group gear" button
// TODO: fire the network request prior to mounting
// TODO: delete item
export default function App(_: any) {
  const [items, setItems] = useState([]);
  const [isOpen, setIsOpen] = useState(false);

  React.useEffect(() => {
    BackendApi.getSearchGearItems("", (r: any) => {
      setItems(r);
    }, (e: any) => {
      console.log("error " + e);
    });
  }, []);

  const plusFunction = useCallback((event) => {
    if (event.keyCode === 187) {
      setIsOpen(true);
    }
  }, []);

  useEffect(() => {
    document.addEventListener("keydown", plusFunction);
    return () => { document.removeEventListener("keydown", plusFunction); };
  }, [plusFunction]);

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
          <Button variant="outlined" onClick={() => setIsOpen(true)}>
            Add gear item [+]
          </Button>
          <AddGearItemsForm
            isOpen={isOpen}
            onClose={() => setIsOpen(false)}
          />
        </div>
       <p>
          Items:
        </p>
        <GearItemsTable gearItems={items}/>
      </header>
    </div>
  );
}
