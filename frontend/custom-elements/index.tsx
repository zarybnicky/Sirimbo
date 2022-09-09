import * as ReactDOM from 'react-dom';
import { AnnouncementListElement } from './announcement-list';
import { ArticleAdminListElement } from './articles-list';
import { DateRangeElement, DateElement } from './date';
import { EventListElement } from './event-list';
import { EventParticipantExportElement } from './event-participant-export';
import { CohortExportElement } from './cohort-export';
import { GalleryDirectoryListElement } from './gallery-directory-list';
import { ReservationAdminListElement } from './reservation-list';
import { ReservationSelectElement } from './reservation-select';
import { RozpisAdminListElement } from './schedule-list';
import { ReactPageElement } from './react-page';
import { QRPayment } from '../components/QRPayment';

class QRCodeElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(<QRPayment {
      ...{
        acc: this.getAttribute('acc') || '',
        am: this.getAttribute('am') || '',
        msg: this.getAttribute('msg') || '',
        ss: this.getAttribute('ss') || '',
        vs: this.getAttribute('vs') || '',
        ks: this.getAttribute('ks') || '',
      }
    } />, this);
  }
}

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
customElements.define('react-page', ReactPageElement);
customElements.define('qr-payment', QRCodeElement);
