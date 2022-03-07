import React, { useCallback, useEffect, useState } from 'react';

import './App.css';
import * as BackendApi from './backend_api.js';

import Button from '@mui/material/Button';
import Dialog from '@mui/material/Dialog';
import DialogActions from '@mui/material/DialogActions';
import DialogContent from '@mui/material/DialogContent';
import DialogContentText from '@mui/material/DialogContentText';
import DialogTitle from '@mui/material/DialogTitle';
import InputAdornment from '@mui/material/InputAdornment';
import MenuItem from '@mui/material/MenuItem';
import TextField from '@mui/material/TextField';


interface Props {
  isNewTrip: boolean;
  isOpen: boolean;
  onClose: () => void;
}

export default function TripForm(props: Props) {
  const [people, setPeople] = useState([""]);
  const [tripName, setTripName] = useState("");

  const handleTripNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setTripName(e.target.value);
  };

  const getHandlePersonNameChange = (oldName: string) => {
    return (e: React.ChangeEvent<HTMLInputElement>) => {
      const newPeople = people.map((p) => {
        if (p === oldName) {
          return e.target.value;
        } else {
          return p;
        }
      });
      setPeople(newPeople);
    };
  };

  const handleNewPerson = (_: React.SyntheticEvent) => {
    const newPeople:Array<string> = [...people];
    newPeople.push("");
    setPeople(newPeople);
  };

  // TODO
  const handleSubmit = (event: React.SyntheticEvent) => {
    event.preventDefault();
    BackendApi.postPutGearItem(undefined, (r: any) => {
      console.log("createOrEditTrip success: " + r);
    }, (e: any) => {
      console.log("createOrEditTrip error: " + e);
    });

    return false;
  };

  // TODO: autofocus last row
  const peopleRows = [];
  for (var i = 0; i < people.length; i++) {
    peopleRows.push(
      <TextField
        margin="dense"
        fullWidth
        label={"Person #" + (i + 1)}
        id={"person" + i}
        key={"person" + i}
        variant="standard"
        value={people[i]}
        onChange={getHandlePersonNameChange(people[i])}
      />
    );
  }

  const verb = props.isNewTrip ? "Create" : "Edit"
  return (
    <Dialog
        maxWidth="sm"
        fullWidth={true}
        onClose={props.onClose}
        open={props.isOpen}>
      <DialogTitle>{verb} trip</DialogTitle>
      <DialogContent>
        <TextField
          autoFocus
          margin="dense"
          label="Trip name"
          key="Trip name"
          fullWidth
          variant="standard"
          value={tripName}
          onChange={handleTripNameChange}
        />
        {peopleRows}
        <DialogActions>
          <Button onClick={handleNewPerson}>Add Person</Button>
          <Button onClick={handleSubmit}>Save</Button>
          <Button onClick={props.onClose}>Cancel</Button>
        </DialogActions>
      </DialogContent>
    </Dialog>
  );
}
