import React from 'react';
import PropTypes from 'prop-types';

import './App.css';
import { GearItem, GearKind } from './types';

import { DataGrid, GridColDef } from '@mui/x-data-grid';
import { ThemeProvider } from '@emotion/react';
import { createTheme } from '@mui/material/styles';


export interface GearItemsProps {
  gearItems: Array<GearItem>;
}

class GearItemsTable extends React.Component<GearItemsProps> {
  render() {
    const columns: GridColDef[] = [
      { field: "name", headerName: "Name", width: 600 },
      { field: "mass", headerName: "Mass", width: 100 },
      { field: "isPersonal", headerName: "Personal", width: 100 },
      { field: "kind", headerName: "Kind", width: 100 },
    ];
    const theme = createTheme({
      palette: {
        mode: "dark",
      }
    });
    return <ThemeProvider theme={theme}>
      <div style={{ height: 3000, width: 900 }}>
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

export default GearItemsTable;
