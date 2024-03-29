import React, { useCallback, useEffect, useState } from 'react';

import './App.css';
import * as BackendApi from './client';

import Button from '@mui/material/Button';
import Dialog from '@mui/material/Dialog';
import DialogActions from '@mui/material/DialogActions';
import DialogContent from '@mui/material/DialogContent';
import DialogContentText from '@mui/material/DialogContentText';
import DialogTitle from '@mui/material/DialogTitle';
import InputAdornment from '@mui/material/InputAdornment';
import MenuItem from '@mui/material/MenuItem';
import TextField from '@mui/material/TextField';

import { GearItem, GearKind } from './client.d';

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

interface FormState {
  isOpen: boolean;
  gearItem: GearItem;
}

interface Props {
  isNewItem: boolean;
  isOpen: boolean;
  onClose: Function;
  gearItem: GearItem;
  editFormState: (formState: FormState) => void;
}

// TODO: form validation
// TODO: function components
// TODO: Doom emacs tide
// TODO: doom/modules/readme
// TODO: I'm probably in a jsx mode when I should be in a tsx mode
//       (which is maybe a minor mode?)
export default function GearItemForm(props: Props) {
  const editFormState = (edit: Function) => {
    var newGearItem = {
      ...props.gearItem
    };
    edit(newGearItem);
    props.editFormState({
      isOpen: props.isOpen,
      gearItem: newGearItem
    });
  }

  const setName = (value: string) => {
    editFormState((newGearItem: GearItem) => {
      newGearItem.name = value;
    });
  }

  const setIsPersonal = (value: boolean) => {
    editFormState((newGearItem: GearItem) => {
      newGearItem.isPersonal = value;
    });
  }

  const setOz = (value: number) => {
    editFormState((newGearItem: GearItem) => {
      newGearItem.oz = value;
    });
  }

  const setKind = (value: GearKind) => {
    editFormState((newGearItem: GearItem) => {
      newGearItem.kind = value;
    });
  }

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

  const setCleanState = (isOpen: boolean) => {
    props.editFormState({
      isOpen: isOpen,
      gearItem: getCleanGearItem()
    });
  };

  // TODO: not calling props.onClose here; this is OK but can diverge
  const onClose = () => {
    setCleanState(false);
  };

  const handleSubmit = (event: React.SyntheticEvent) => {
    event.preventDefault();
    BackendApi.postPutgearitem(props.gearItem).then((r) => {
      console.log("createGearItem success: " + r);
      setCleanState(true);
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

  const verb = props.isNewItem ? "Add" : "Edit"

  return (
    <Dialog onClose={onClose} open={props.isOpen}>
      <DialogTitle>{verb} gear item</DialogTitle>
      <DialogContent>
        <TextField
          autoFocus
          margin="dense"
          id="name"
          label="Item name"
          fullWidth
          variant="standard"
          value={props.gearItem.name}
          onChange={handleNameChange}
        />
        <TextField
          margin="dense"
          id="isPersonal"
          label="Is personal"
          variant="standard"
          value={props.gearItem.isPersonal}
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
          value={props.gearItem.oz === -1 ? "" : props.gearItem.oz}
          onChange={handleOzChange}
        /><br/>
        <TextField
          select
          margin="dense"
          id="kind"
          label="kind"
          variant="standard"
          value={props.gearItem.kind}
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
