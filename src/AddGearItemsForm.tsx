import React from 'react';

import './App.css';
import * as BackendApi from './backend_api.js';

import Button from '@mui/material/Button';
import Dialog from '@mui/material/Dialog';
import DialogTitle from '@mui/material/DialogTitle';

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
      oz: 0,
      kind: "Base",
    };

    this.handleNameChange = this.handleNameChange.bind(this);
    this.handleIsPersonalChange = this.handleIsPersonalChange.bind(this);
    this.handleOzChange = this.handleOzChange.bind(this);
    this.handleKindChange = this.handleKindChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
  }

  handleNameChange(e: React.ChangeEvent<HTMLInputElement>) {
    this.setState({name: e.target.value});
    console.log("state: " + this.state);
  }

  handleIsPersonalChange(e: React.ChangeEvent<HTMLInputElement>) {
    this.setState({isPersonal: e.target.value === "true"});
    console.log("state: " + this.state);
  }

  handleOzChange(e: React.ChangeEvent<HTMLInputElement>) {
    this.setState({oz: parseFloat(e.target.value)});
    console.log("state: " + this.state);
  }

  handleKindChange(e: React.ChangeEvent<HTMLInputElement>) {
    this.setState({kind: e.target.value as GearKind});
    console.log("state: " + this.state);
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
    return (
      <Dialog
          onClose={this.props.onClose}
          open={this.props.isOpen}>
        <DialogTitle>Add gear item</DialogTitle>
        <form onSubmit={this.handleSubmit}>
          <label>
            Name:
            <input type="text" defaultValue={this.state.name} onChange={this.handleNameChange} />
          </label>
          <label>
            isPersonal:
          <input type="checkbox" defaultValue={this.state.isPersonal ? "a" : ""} onChange={this.handleIsPersonalChange} />
          </label>
          <label>
            oz:
            <input type="text" defaultValue={this.state.oz} onChange={this.handleOzChange} />
          </label>
          <label>
            kind:
            <input type="text" defaultValue={this.state.kind} onChange={this.handleKindChange} />
          ,    </label>
          <input type="submit" value="Submit" />
        </form>
      </Dialog>
    );
  }
}

interface SimpleDialogProps {
  isOpen: boolean;
  onClose: any;
}

export default AddGearItemsForm
