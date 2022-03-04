import React from 'react';

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


interface GearItemsFormState {
  name: string;
  isPersonal: boolean;
  oz: number;
  kind: GearKind;
}

// TODO: modal
// TODO: buttons: 1/ Save & New 2/ Save 3/ Cancel
// TODO: form validation
// TODO: buttons: 1/ Save & New 2/ Save 3/ Cancel
// TODO: form validation
// TODO: function components
// TODO: Doom emacs tide
// TODO: doom/modules/readme
// TODO: I'm probably in a jsx mode when I should be in a tsx mode
//       (which is maybe a minor mode?)
class AddGearItemsForm extends React.Component<SimpleDialogProps, GearItemsFormState> {
  constructor(props: any) {
    super(props);
    this.state = {
      name: "",
      isPersonal: true,
      oz: -1,
      kind: "Base",
    };

    this.handleNameChange = this.handleNameChange.bind(this);
    this.handleIsPersonalChange = this.handleIsPersonalChange.bind(this);
    this.handleOzChange = this.handleOzChange.bind(this);
    this.handleKindChange = this.handleKindChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
    this.handleSubmitAndClose = this.handleSubmitAndClose.bind(this);
  }

  handleNameChange(e: React.ChangeEvent<HTMLInputElement>) {
    this.setState({name: e.target.value});
  }

  handleIsPersonalChange(e: React.ChangeEvent<HTMLInputElement>) {
    this.setState({isPersonal: e.target.value === "true"});
  }

  handleOzChange(e: React.ChangeEvent<HTMLInputElement>) {
    this.setState({oz: parseFloat(e.target.value)});
  }

  handleKindChange(e: React.ChangeEvent<HTMLInputElement>) {
    this.setState({kind: e.target.value as GearKind});
  }

  handleSubmitAndClose(event: React.SyntheticEvent) {
    this.handleSubmit(event);
    this.props.onClose();
  }

  handleSubmit(event: React.SyntheticEvent) {
    event.preventDefault();
    const payload = {
      itemId: "",
      name: this.state.name,
      isPersonal: this.state.isPersonal,
      oz: this.state.oz,
      kind: this.state.kind,
      creatorUserId: "",
    }
    BackendApi.postPutGearItem(payload, (r: any) => {
      console.log("createGearItem success: " + r);
    }, (e: any) => {
      console.log("createGearItem error: " + e);
    });
    return false;
  }

  render() {
    // TODO: reference GearKind
    const kinds = ["Base", "Technical", "Clothing", "Electronic", "Nutrition"];
    return (
      <Dialog onClose={this.props.onClose} open={this.props.isOpen}>
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
            value={this.state.name}
            onChange={this.handleNameChange}
          />
          <TextField
            autoFocus
            margin="dense"
            id="isPersonal"
            label="Is personal"
            variant="standard"
            value={this.state.isPersonal}
            onChange={this.handleIsPersonalChange}
          /><br/>
          <TextField
            autoFocus
            margin="dense"
            id="oz"
            label="Mass"
            variant="standard"
            InputProps={{
              endAdornment: <InputAdornment position="end">oz</InputAdornment>
            }}
            value={this.state.oz === -1 ? "" : this.state.oz}
            onChange={this.handleOzChange}
          /><br/>
          <TextField
            autoFocus
            select
            margin="dense"
            id="kind"
            label="kind"
            variant="standard"
            value={this.state.kind}
            onChange={this.handleKindChange}
          >
          {kinds.map((kind) => (
            <MenuItem key={kind} value={kind}>
              {kind}
            </MenuItem>
          ))}
          </TextField>
        <DialogActions>
          <Button onClick={this.handleSubmit}>Save & New</Button>
          <Button onClick={this.handleSubmitAndClose}>Save</Button>
          <Button onClick={this.props.onClose}>Cancel</Button>
        </DialogActions>
        </DialogContent>
      </Dialog>
    );
  }
}

interface SimpleDialogProps {
  isOpen: boolean;
  onClose: any;
}

export default AddGearItemsForm;
