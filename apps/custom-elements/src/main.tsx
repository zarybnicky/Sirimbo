import 'bootstrap';
import './leaflet';
import './video.min';

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
import { DateRangeElement } from './custom-elements/date';
import { EventListElement } from './custom-elements/event-list';
import { GalleryDirectoryListElement } from './custom-elements/gallery-directory-list';
import { ReservationAdminListElement } from './custom-elements/reservation-list';
import { ReservationSelectElement } from './custom-elements/reservation-select';
import { RozpisAdminListElement } from './custom-elements/schedule-list';
import { PrijdTancitElement } from './custom-elements/prijd-tancit';

customElements.define('announcement-list', AnnouncementListElement);
customElements.define('article-admin-list', ArticleAdminListElement);
customElements.define('date-range', DateRangeElement);
customElements.define('event-list', EventListElement);
customElements.define('gallery-directory-list', GalleryDirectoryListElement);
customElements.define('reservation-admin-list', ReservationAdminListElement);
customElements.define('reservation-select', ReservationSelectElement);
customElements.define('rozpis-admin-list', RozpisAdminListElement);
customElements.define('prijd-tancit', PrijdTancitElement);