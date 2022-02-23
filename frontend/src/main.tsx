import * as React from 'react';
import * as ReactDOM from 'react-dom';

import 'bootstrap';
import './bundles/leaflet';
import './bundles/spectrum';
import './bundles/video.min';
import './bundles/main';

import 'leaflet/dist/leaflet.css';
import './style/index.scss';

import L from 'leaflet';
delete (L.Icon.Default.prototype as unknown as any)._getIconUrl;
L.Icon.Default.mergeOptions({
  iconRetinaUrl: require('leaflet/dist/images/marker-icon-2x.png').default,
  iconUrl: require('leaflet/dist/images/marker-icon.png').default,
  shadowUrl: require('leaflet/dist/images/marker-shadow.png').default,
});

import { AnnouncementListElement } from './custom-elements/announcement-list';
import { ArticleAdminListElement } from './custom-elements/articles-list';
import { DateRangeElement, DateElement } from './custom-elements/date';
import { EventListElement } from './custom-elements/event-list';
import { EventParticipantExportElement } from './custom-elements/event-participant-export';
import { CohortExportElement } from './custom-elements/cohort-export';
import { GalleryDirectoryListElement } from './custom-elements/gallery-directory-list';
import { ReservationAdminListElement } from './custom-elements/reservation-list';
import { ReservationSelectElement } from './custom-elements/reservation-select';
import { RozpisAdminListElement } from './custom-elements/schedule-list';

customElements.define('announcement-list', AnnouncementListElement);
customElements.define('article-admin-list', ArticleAdminListElement);
customElements.define('date-range', DateRangeElement);
customElements.define('single-date', DateElement);
customElements.define('event-list', EventListElement);
customElements.define('gallery-directory-list', GalleryDirectoryListElement);
customElements.define('reservation-admin-list', ReservationAdminListElement);
customElements.define('reservation-select', ReservationSelectElement);
customElements.define('rozpis-admin-list', RozpisAdminListElement);
customElements.define('event-participant-export', EventParticipantExportElement);
customElements.define('cohort-export', CohortExportElement);

import { QRPayment } from './components/QRPayment';
class QRCodeElement extends HTMLElement {
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
