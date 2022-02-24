import React from 'react';
import sra from './sun-ribbon-2.jpeg';
import * as BackendApi from './backend_api.js';
import { GearItem } from './types';
import { DataGrid } from '@mui/x-data-grid';
import { createTheme } from '@mui/material/styles';
import './App.css';

class App extends React.Component<{}, { items: Array<GearItem> }> {
  constructor(props: any) {
    super(props);
    this.state = { items: [] };
  }

  componentDidMount() {
    BackendApi.getSearchGearItems("", (r: any) => {
      this.setState({items: r});
    }, (e: any) => {
      console.log("error " + e);
    });
  }

  render() {
    console.log("this.state.items: " + this.state.items);
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
          <GearItems gearItems={this.state.items}/>
        </header>
      </div>
    );
  }
}


export interface GearItemsProps {
  gearItems: Array<GearItem>;
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
      { field: "name", headerName: "Name", width: 150 },
      { field: "isPersonal", headerName: "Personal", width: 150 },
      { field: "oz", headerName: "Oz.", width: 150 },
      { field: "kind", headerName: "Kind", width: 150 },
    ];
    return <div style={{ height: 300, width: '100%' }}>
      <DataGrid
        rows={this.props.gearItems.map(item => ({
          "id": item.itemId,
          "key": item.itemId,
          "name": item.name,
          "isPersonal": item.isPersonal,
          "oz": item.oz,
          "kind": item.kind
        }))}
        columns={columns}
        pageSize={5}
        rowsPerPageOptions={[5]}
      />
    </div>;
  }
}


export default App;
