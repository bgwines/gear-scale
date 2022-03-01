import React from 'react';
import sra from './sun-ribbon-2.jpeg';
import * as BackendApi from './backend_api.js';
import { GearItem, GearKind } from './types';
import { DataGrid, GridColDef } from '@mui/x-data-grid';
import { createTheme } from '@mui/material/styles';
import './App.css';
import { ThemeProvider } from '@emotion/react';

// TODO: "Rebalance group gear" button
class App extends React.Component<{}, { items: Array<GearItem> }> {
  constructor(props: any) {
    super(props);
    this.state = { items: [] };
  }

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
          <p>
            Items:
          </p>
          <GearItemsForm/>
          <br/><br/>
          <GearItems gearItems={this.state.items}/>
        </header>
      </div>
    );
  }
}


export interface GearItemsProps {
  gearItems: Array<GearItem>;
}

interface GearItemsFormState {
  name: string;
  isPersonal: boolean;
  oz: number;
  kind: GearKind;
}

// TODO: modal
// TODO: buttons: 1/ Save & New 2/ Save 3/ Cancel
// TODO: form validation
class GearItemsForm extends React.Component<{}, GearItemsFormState> {
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
        </label>
        <input type="submit" value="Submit" />
      </form>
    );
  }
}

class GearItems extends React.Component<GearItemsProps> {
  render() {
    const columns: GridColDef[] = [
      { field: "name", headerName: "Name", width: 600 },
      { field: "mass", headerName: "Mass", width: 150 },
      { field: "isPersonal", headerName: "Personal", width: 150 },
      { field: "kind", headerName: "Kind", width: 150 },
    ];
    const theme = createTheme({
      palette: {
        mode: "dark",
      }
    });
    return <ThemeProvider theme={theme}>
      <div style={{ height: 3000, width: '100%' }}>
        <DataGrid
          rows={this.props.gearItems.map(item => ({
            "id": item.itemId,
            "key": item.itemId,
            "name": item.name,
            "isPersonal": item.isPersonal,
            "mass": this.displayMass(item.oz),
            "kind": item.kind
          }))}
          columns={columns}
          pageSize={100}
          rowsPerPageOptions={[4]}
        />
      </div>
    </ThemeProvider>;
  }

  displayMass(totalOz: number): string {
    var oz = Number(Number(totalOz % 16).toFixed(1))
    if (oz % 1 == 0) {
      oz = Number(oz.toFixed(0));
    }

    const lbs = Math.floor(totalOz / 16)
    if (lbs == 0) {
      return oz + "oz"
    } else if (lbs == 1) {
      return lbs + "lb " + oz + "oz"
    } else {
      return lbs + "lbs " + oz + "oz"
    }
  }
}


export default App;
