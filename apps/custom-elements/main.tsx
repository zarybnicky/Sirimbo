import 'bootstrap';
import './style/index.scss';
import 'react-toastify/dist/ReactToastify.css';

import React from 'react';
import { Client, Provider } from 'urql';
import { configureUrql } from '@app/graphql/query';
import { ToastContainer } from 'react-toastify';
import { ProvideAuth } from '@app/ui/use-auth';

import r2wc from '@r2wc/react-to-web-component';
import AnnouncementList from './custom-elements/announcement-list';
import ArticleAdminList from './custom-elements/articles-list';
import DateRange from './custom-elements/date';
import EventAdminList from './custom-elements/event-list';
import GalleryDirectoryList from './custom-elements/gallery-directory-list';
import ReservationAdminList from './custom-elements/reservation-list';
import ReservationSelect from './custom-elements/reservation-select';
import RozpisAdminList from './custom-elements/schedule-list';
import Contact from '@app/ui/Contact';
import Map from '@app/map/Map-client';
import RichTextEditor from '@app/editor/RichTextEditor';
import { UserList } from '@app/ui/UserList';
import { ScheduleView } from '@app/ui/ScheduleView';
import { EventMemberList } from '@app/ui/EventMemberList';
import { ArticlePublicList } from '@app/ui/ArticlePublicList';
import { EventItem } from '@app/ui/EventItem';
import { CoupleView } from '@app/ui/CoupleView';
import { AnnouncementList as AnnouncementAdminList, CoupleList } from '@app/ui/entity-lists';
import { ConfirmProvider } from '@app/ui/Confirm';

const client = new Client(configureUrql());
const withProviders =
  (Page: React.ElementType<any>) =>
  (props: any) => (
    <Provider value={client}>
      <ProvideAuth>
        <ConfirmProvider>
          <Page {...props} />
          <ToastContainer limit={3} />
        </ConfirmProvider>
      </ProvideAuth>
    </Provider>
  );

customElements.define('announcement-list', r2wc(withProviders(AnnouncementList)));
customElements.define('article-admin-list', r2wc(withProviders(ArticleAdminList)));
customElements.define('couple-admin-list', r2wc(withProviders(CoupleList)));
customElements.define('couple-view', r2wc(withProviders(CoupleView), {
  props: { id: "string" },
}));
customElements.define('event-admin-list', r2wc(withProviders(EventAdminList)));
customElements.define('event-member-list', r2wc(withProviders(EventMemberList), {
  props: { selected: "string" },
}));
customElements.define('event-item', r2wc(withProviders(EventItem), {
  props: { id: "string" },
}));
customElements.define('gallery-directory-list', r2wc(withProviders(GalleryDirectoryList)));
customElements.define('reservation-admin-list', r2wc(withProviders(ReservationAdminList)));
customElements.define('nastenka-admin-list', r2wc(withProviders(AnnouncementAdminList)));
customElements.define('user-list', r2wc(withProviders(UserList)));
customElements.define('schedule-view', r2wc(withProviders(ScheduleView)));
customElements.define('reservation-select', r2wc(withProviders(ReservationSelect)));
customElements.define('rozpis-admin-list', r2wc(withProviders(RozpisAdminList)));
customElements.define('olymp-contact', r2wc(withProviders(Contact)));
customElements.define('olymp-articles', r2wc(withProviders(ArticlePublicList)));
customElements.define('ck-editor', r2wc(RichTextEditor, {
  props: {
    name: 'string',
    initialstate: 'string',
  },
}));
customElements.define('date-range', r2wc(DateRange, {
  props: {
    noYear: 'boolean',
    from: 'string',
    to: 'string',
  },
}));

function CustomMap({ places }: { places: string }) {
  const holeckova = { lat: 49.57963, lng: 17.2495939 };
  const sgo = { lat: 49.5949, lng: 17.2634 };
  const median = { lat: 49.58727525, lng: 17.25661055 };

  const center = places === 'both' ? median : places === 'sgo' ? sgo : holeckova;
  const zoom = places === 'both' ? 12 : 14;
  return (
    <Map>
      {({ TileLayer, Marker, MapContainer, Popup }) => (
        <MapContainer
          className="h-48 min-w-24"
          center={center}
          zoom={zoom}
          scrollWheelZoom={false}
        >
          <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
          {places !== 'sgo' && (
            <Marker position={holeckova}>
              <Popup>ZŠ Holečkova</Popup>
            </Marker>
          )}
          {places !== 'holeckova' && (
            <Marker position={sgo}>
              <Popup>SGO</Popup>
            </Marker>
          )}
        </MapContainer>
      )}
    </Map>
  );
}

customElements.define(
  'leaflet-map',
  r2wc(CustomMap, {
    props: {
      places: 'string',
    },
  }),
);
