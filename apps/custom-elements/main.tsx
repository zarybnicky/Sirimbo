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
import EventList from './custom-elements/event-list';
import GalleryDirectoryList from './custom-elements/gallery-directory-list';
import ReservationAdminList from './custom-elements/reservation-list';
import ReservationSelect from './custom-elements/reservation-select';
import RozpisAdminList from './custom-elements/schedule-list';
import PrijdTancit from '@app/branding-olymp/PrijdTancit';
import Contact from '@app/branding-olymp/Contact';
import Map from '@app/map/Map-client';
import RichTextEditor from '@app/editor/RichTextEditor';
import { UserList } from '@app/ui/UserList';
import { ScheduleView } from '@app/ui/ScheduleView';
import { AnnouncementList as AnnouncementAdminList } from '@app/ui/entity-lists';

const client = new Client(configureUrql());
const withProviders =
  <T extends JSX.IntrinsicAttributes>(Page: React.JSXElementConstructor<T>) =>
  (props: T) => (
    <Provider value={client}>
      <ProvideAuth>
        <Page {...props} />
        <ToastContainer limit={3} />
      </ProvideAuth>
    </Provider>
  );

import { useQuery } from 'urql';
import { Pagination } from '@app/ui/Pagination';
import { ArticleCard } from '@app/ui/cards/ArticleCard';
import { slugify } from '@app/ui/slugify';
import { ArticlesDocument } from '@app/graphql/Articles';

function ArticleList() {
  const [page, setPage] = React.useState(1);
  const [{ data }] = useQuery({query: ArticlesDocument, variables: { first: 12, offset: (page - 1) * 12 }});
  return (
    <>
      <div className="col-feature grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mb-6">
        {data?.aktualities?.nodes.map((x) => (
          <ArticleCard
            key={x.id}
            header={x.atJmeno}
            href={`/articles/${x.id}/${slugify(x.atJmeno)}`}
            img={`https://tkolymp.cz/galerie/${x.galerieFotoByAtFotoMain?.gfPath}`}
            preview={x.atPreview}
          />
        ))}
      </div>
      <Pagination total={data?.aktualities?.totalCount || 0} limit={12} page={page} setPage={setPage} />
    </>
  );
}

customElements.define('announcement-list', r2wc(withProviders(AnnouncementList)));
customElements.define('article-admin-list', r2wc(withProviders(ArticleAdminList)));
customElements.define('event-list', r2wc(withProviders(EventList)));
customElements.define('gallery-directory-list', r2wc(withProviders(GalleryDirectoryList)));
customElements.define('reservation-admin-list', r2wc(withProviders(ReservationAdminList)));
customElements.define('nastenka-admin-list', r2wc(withProviders(AnnouncementAdminList)));
customElements.define('user-list', r2wc(withProviders(UserList)));
customElements.define('schedule-view', r2wc(withProviders(ScheduleView)));
customElements.define('reservation-select', r2wc(withProviders(ReservationSelect)));
customElements.define('rozpis-admin-list', r2wc(withProviders(RozpisAdminList)));
customElements.define('prijd-tancit', r2wc(withProviders(PrijdTancit)));
customElements.define('olymp-contact', r2wc(withProviders(Contact)));
customElements.define('olymp-articles', r2wc(withProviders(ArticleList)));
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
