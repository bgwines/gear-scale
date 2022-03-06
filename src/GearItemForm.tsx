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

import { GearItem, GearKind } from './types';


interface Props {
  isOpen: boolean;
  onClose: any;
  initialState?: GearItem;
}

// TODO: form validation
// TODO: function components
// TODO: Doom emacs tide
// TODO: doom/modules/readme
// TODO: I'm probably in a jsx mode when I should be in a tsx mode
//       (which is maybe a minor mode?)
export default function GearItemForm(props: Props) {
  console.log("props.isOpen: " + props.isOpen);
  console.log("props.initialState: " + props.initialState);

  const cleanState = (): GearItem => {
    return {
      itemId: "",
      name: "",
      isPersonal: true,
      oz: -1,
      kind: "Base",
      creatorUserId: "",
    };
  };

  const initialState = (props.initialState !== undefined ?
    props.initialState : cleanState());

  const [itemId, setItemId] = useState(initialState.itemId);
  const [name, setName] = useState(initialState.name);
  const [isPersonal, setIsPersonal] = useState(initialState.isPersonal);
  const [oz, setOz] = useState(initialState.oz);
  const [kind, setKind] = useState(initialState.kind);
  const [creatorUserId, setCreatorUserId] = useState(initialState.creatorUserId);

  const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.value !== "+") {
      // Plus key opens the dialog, but also populates the `name` field since
      // that's the default-focused field
      setName(e.target.value);
    }
  };

  const handleIsPersonalChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.value) {
      const ch = e.target.value[e.target.value.length - 1];
      if (ch === 't') {
        setIsPersonal(true);
      } else if (ch === 'f') {
        setIsPersonal(false);
      }
    }
  };

  const handleOzChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setOz(parseFloat(e.target.value));
  };

  const handleKindChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setKind(e.target.value as GearKind);
  };

  const setInitialState = () => {
    const state = cleanState();
    setItemId(state.itemId);
    setName(state.name);
    setIsPersonal(state.isPersonal);
    setOz(state.oz);
    setKind(state.kind);
    setCreatorUserId(state.creatorUserId);
  };

  const onClose = () => {
    setInitialState();
    props.onClose();
  };

  const handleSubmit = (event: React.SyntheticEvent) => {
    event.preventDefault();
    const payload = {
      itemId: "",
      name: name,
      isPersonal: isPersonal,
      oz: oz,
      kind: kind,
      creatorUserId: "",
    }
    BackendApi.postPutGearItem(payload, (r: any) => {
      console.log("createGearItem success: " + r);
      setInitialState();
    }, (e: any) => {
      console.log("createGearItem error: " + e);
    });

    // TODO focus first elem
    // TODO sort
    return false;
  }

  const handleSubmitAndClose = (event: React.SyntheticEvent) => {
    handleSubmit(event);
    onClose();
  }

  // TODO: reference GearKind
  const kinds = ["Base", "Technical", "Clothing", "Electronic", "Nutrition"];
  return (
    <Dialog onClose={onClose} open={props.isOpen}>
      <DialogTitle>Add gear item</DialogTitle>
      <DialogContent>
      <DialogContentText>
      This is the dialog where you can enter a new gear item
      </DialogContentText>
        <TextField
          autoFocus
          margin="dense"
          id="name"
          label="Item name"
          fullWidth
          variant="standard"
          value={name}
          onChange={handleNameChange}
        />
        <TextField
          margin="dense"
          id="isPersonal"
          label="Is personal"
          variant="standard"
          value={isPersonal}
          onChange={handleIsPersonalChange}
        /><br/>
        <TextField
          margin="dense"
          id="oz"
          label="Mass"
          variant="standard"
          InputProps={{
            endAdornment: <InputAdornment position="end">oz</InputAdornment>
          }}
          value={oz === -1 ? "" : oz}
          onChange={handleOzChange}
        /><br/>
        <TextField
          select
          margin="dense"
          id="kind"
          label="kind"
          variant="standard"
          value={kind}
          onChange={handleKindChange}
        >
        {kinds.map((kind) => (
          <MenuItem key={kind} value={kind}>
            {kind}
          </MenuItem>
        ))}
        </TextField>
      <DialogActions>
        <Button onClick={handleSubmit}>Save & New</Button>
        <Button onClick={handleSubmitAndClose}>Save</Button>
        <Button onClick={onClose}>Cancel</Button>
      </DialogActions>
      </DialogContent>
    </Dialog>
  );
}
