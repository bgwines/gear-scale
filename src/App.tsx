import React from 'react';
import sra from './sun-ribbon-2.jpeg';
import * as BackendApi from './backend_api.js';
import { GearItem } from './types';
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
    console.log("this.props.gearItems: " + this.props.gearItems);
    return (
      <ul>
      {this.props.gearItems.map(item => (
        <li key={item.itemId}>{item.name}</li>
      ))}
      </ul>
    );
  }
}


export default App;
