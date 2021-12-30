import * as React from 'react';
import * as ReactDOM from 'react-dom';

import 'bootstrap';
import './bundles/leaflet';
import './bundles/spectrum';
import './bundles/video.min';
import './bundles/main';

import 'leaflet/dist/leaflet.css';
import './style/index.scss';

import { QRPayment } from './components/QRPayment';

import L from 'leaflet';
delete (L.Icon.Default.prototype as unknown as any)._getIconUrl;
L.Icon.Default.mergeOptions({
  iconRetinaUrl: require('leaflet/dist/images/marker-icon-2x.png').default,
  iconUrl: require('leaflet/dist/images/marker-icon.png').default,
  shadowUrl: require('leaflet/dist/images/marker-shadow.png').default,
});

export class QRCodeElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(<QRPayment {...{
      acc: this.getAttribute('acc') || '',
      am: this.getAttribute('am') || '',
      msg: this.getAttribute('msg') || '',
      ss: this.getAttribute('ss') || '',
      vs: this.getAttribute('vs') || '',
      ks: this.getAttribute('ks') || '',
    }} />, this);
  }
}
customElements.define('qr-payment', QRCodeElement);
