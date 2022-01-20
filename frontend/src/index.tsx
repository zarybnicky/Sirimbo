import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { App } from './app';

// Ideally include in ReactPage.tsx, to minimize bundle size
import '@react-page/editor/lib/index.css';
import '@react-page/plugins-slate/lib/index.css';
import '@react-page/plugins-image/lib/index.css';

import 'leaflet/dist/leaflet.css';

import L from 'leaflet';
delete (L.Icon.Default.prototype as unknown as any)._getIconUrl;
L.Icon.Default.mergeOptions({
  iconRetinaUrl: require('leaflet/dist/images/marker-icon-2x.png').default,
  iconUrl: require('leaflet/dist/images/marker-icon.png').default,
  shadowUrl: require('leaflet/dist/images/marker-shadow.png').default,
});


if (document.getElementById('app-new')) {
  ReactDOM.render(<App />, document.getElementById('app-new'));
}

if ((module as any).hot) {
  (module as any).hot.accept();
}
