import React from 'react';
import PropTypes from 'prop-types';

import './App.css';
import sra from './sun-ribbon-2.jpeg';
import * as BackendApi from './backend_api.js';
import {AddGearItemsForm, SimpleDialog} from './AddGearItemsForm';
import GearItemsTable from './GearItemsTable';
import { GearItem, GearKind } from './types';

import Button from '@mui/material/Button';

// TODO: "Rebalance group gear" button
class App extends React.Component<{}, { items: Array<GearItem>, isOpen: boolean }> {
  constructor(props: any) {
    super(props);
    this.state = {
      items: [],
      isOpen: false,
    };

    this.handleClickOpen = this.handleClickOpen.bind(this);
    this.handleClose = this.handleClose.bind(this);
  }

  handleClickOpen() {
    this.setState({isOpen: true});
  };

  handleClose(value: any) {
    this.setState({isOpen: false});
  };

  // TODO: fire the network request prior to mounting
  componentDidMount() {
    BackendApi.getSearchGearItems("", (r: any) => {
      this.setState({items: r});
    }, (e: any) => {
      console.log("error " + e);
    });
  }

  render() {
    return (
      <div className="App">
        <header className="App-header">
          <img src={sra}
            className="Sun-Ribbon"
            width="1000px"
            alt="Sun Ribbon Arete" />

          <br/>
          <div>
            <Button variant="outlined" onClick={this.handleClickOpen}>
              Add gear item
            </Button>
            <SimpleDialog
              isOpen={this.state.isOpen}
              onClose={this.handleClose}
            />
          </div>

          <p>
            Items:
          </p>
          <GearItemsTable gearItems={this.state.items}/>
        </header>
      </div>
    );
  }
}


export default App;
