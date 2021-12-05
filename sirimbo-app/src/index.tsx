import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { App } from './app';

import 'leaflet/dist/leaflet.css';

import L from 'leaflet';
delete (L.Icon.Default.prototype as unknown as any)._getIconUrl;
L.Icon.Default.mergeOptions({
  iconRetinaUrl: require('leaflet/dist/images/marker-icon-2x.png'),
  iconUrl: require('leaflet/dist/images/marker-icon.png'),
  shadowUrl: require('leaflet/dist/images/marker-shadow.png'),
});


if (document.getElementById('app-new')) {
  ReactDOM.render(<App />, document.getElementById('app-new'));
}
