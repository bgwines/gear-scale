import React from 'react';
import sra from './sun-ribbon-2.jpeg';
import * as BackendApi from './backend_api.js';
import { GearItem } from './types';
import { DataGrid } from '@mui/x-data-grid';
import { createTheme } from '@mui/material/styles';
import './App.css';

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

// TODO: modal
// TODO: buttons: 1/ Save & New 2/ Save 3/ Cancel
// TODO: form validation
class GearItemsForm extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      name: "",
      isPersonal: true,
      oz: "",
      kind: "Base",
    };

    this.handleNameChange = this.handleNameChange.bind(this);
    this.handleIsPersonalChange = this.handleIsPersonalChange.bind(this);
    this.handleOzChange = this.handleOzChange.bind(this);
    this.handleKindChange = this.handleKindChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
  }

  handleNameChange(event) {
    this.setState({name: event.target.value});
    console.log("state: " + this.state);
  }

  handleIsPersonalChange(event) {
    this.setState({isPersonal: event.target.value});
    console.log("state: " + this.state);
  }

  handleOzChange(event) {
    this.setState({oz: event.target.value});
    console.log("state: " + this.state);
  }

  handleKindChange(event) {
    this.setState({handle: event.target.value});
    console.log("state: " + this.state);
  }

  handleSubmit(event) {
    event.preventDefault();
    const payload = {
      itemId: "",
      name: this.state.name,
      isPersonal: this.state.isPersonal === "true",
      oz: parseFloat(this.state.oz),
      kind: this.state.kind,
      creatorUserId: "",
    }
    BackendApi.postPutGearItem(payload, (r) => {
      console.log("createGearItem success: " + r);
    }, (e) => {
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
          <input type="checkbox" defaultValue={this.state.isPersonal} onChange={this.handleIsPersonalChange} />
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
    //const theme = createTheme({
    //  components: {
    //    MuiDataGrid: {
    //      styleOverrides: {
    //        root: {
    //          backgroundColor: 'red',
    //        },
    //      },
    //    },
    //  },
    //});
    const columns: GridColDef[] = [
      { field: "name", headerName: "Name", width: 600 },
      { field: "mass", headerName: "Mass", width: 150 },
      { field: "isPersonal", headerName: "Personal", width: 150 },
      { field: "kind", headerName: "Kind", width: 150 },
    ];
    return <div style={{ height: 3000, width: '100%' }}>
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
    </div>;
  }

  displayMass(totalOz: float): string {
    const oz = totalOz % 16;
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
