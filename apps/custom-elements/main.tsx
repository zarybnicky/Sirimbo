import 'bootstrap';
import './video.min';
import './style/index.scss';
import 'react-toastify/dist/ReactToastify.css';

import React from 'react';
import { Client, Provider } from 'urql';
import { configureUrql } from '@app/graphql/query';
import { ToastContainer } from 'react-toastify';

import r2wc from '@r2wc/react-to-web-component';
import AnnouncementList from './custom-elements/announcement-list';
import ArticleAdminList from './custom-elements/articles-list';
import DateRange from './custom-elements/date';
import EventList from './custom-elements/event-list';
import GalleryDirectoryList from './custom-elements/gallery-directory-list';
import ReservationAdminList from './custom-elements/reservation-list';
import ReservationSelect from './custom-elements/reservation-select';
import RozpisAdminList from './custom-elements/schedule-list';
import PrijdTancit from './custom-elements/prijd-tancit';
import Map from '@app/map/Map-client';
import RichTextEditor from '@app/editor/RichTextEditor';

const client = new Client(configureUrql());
const withProviders =
  <T extends JSX.IntrinsicAttributes>(Page: React.JSXElementConstructor<T>) =>
  (props: T) => (
    <Provider value={client}>
      <Page {...props} />
      <ToastContainer limit={3} />
    </Provider>
  );

customElements.define('announcement-list', r2wc(withProviders(AnnouncementList)));
customElements.define('article-admin-list', r2wc(withProviders(ArticleAdminList)));
customElements.define('event-list', r2wc(withProviders(EventList)));
customElements.define('gallery-directory-list', r2wc(withProviders(GalleryDirectoryList)));
customElements.define('reservation-admin-list', r2wc(withProviders(ReservationAdminList)));
customElements.define('reservation-select', r2wc(withProviders(ReservationSelect)));
customElements.define('rozpis-admin-list', r2wc(withProviders(RozpisAdminList)));
customElements.define('prijd-tancit', r2wc(withProviders(PrijdTancit)));
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
