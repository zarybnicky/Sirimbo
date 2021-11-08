import 'bootstrap';
import './bundles/leaflet';
import './bundles/spectrum';
import './bundles/video.min';
import './bundles/main';

import './style/index.scss';

import { AnnouncementListElement } from './announcement-list';
import { ArticleAdminListElement } from './articles-list';
import { DateRangeElement, DateElement } from './date';
import { EventListElement } from './event-list';
import { GalleryDirectoryListElement } from './gallery-directory-list';
import { QRCodeElement } from './qr-payment';
import { ReservationAdminListElement } from './reservation-list';
import { ReservationSelectElement } from './reservation-select';
import { RozpisAdminListElement } from './schedule-list';

import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { App } from './app';

customElements.define('announcement-list', AnnouncementListElement);
customElements.define('article-admin-list', ArticleAdminListElement);
customElements.define('date-range', DateRangeElement);
customElements.define('single-date', DateElement);
customElements.define('event-list', EventListElement);
customElements.define('gallery-directory-list', GalleryDirectoryListElement);
customElements.define('qr-payment', QRCodeElement);
customElements.define('reservation-admin-list', ReservationAdminListElement);
customElements.define('reservation-select', ReservationSelectElement);
customElements.define('rozpis-admin-list', RozpisAdminListElement);

console.log(document.getElementById('app-new'));
if (document.getElementById('app-new')) {
    ReactDOM.render(<App />, document.getElementById('app-new'));
}
