import * as React from 'react';
import { Map } from 'components/Map';
import { Card } from 'components/Card';
import classNames from 'classnames';

const CardMap = (props: {
  lat: number;
  lng: number;
  zoom: number;
  name: string;
  className?: string;
}) => {
  return (
    <Map
      className={classNames('map', props.className)}
      center={props}
      zoom={props.zoom}
      scrollWheelZoom={false}
    >
      {({ TileLayer, Marker, Popup }) => (
        <>
          <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
          <Marker position={props}>
            <Popup>{props.name}</Popup>
          </Marker>
        </>
      )}
    </Map>
  );
};

type Location = {
  name: string;
  address: React.ReactNode;
  href: string | null;
  mapHref: string;
  map: {
    lat: number;
    lng: number;
    zoom: number;
  };
};

export const LocationCard = ({ item: x }: { item: Location }) => {
  return (
    <Card className="grid md:grid-cols-[1fr_2fr] gap-4 items-center">
      <CardMap className="h-[200px]" name={x.name} {...x.map} />
      <div className="grow text-gray-800">
        <h5 className="text-red-600 text-xl font-bold">{x.name}</h5>
        <div className="py-2">{x.address}</div>
        {x.href && (
          <a href={x.href} rel="noreferrer" target="_blank" className="block underline">
            {x.href}
          </a>
        )}
        <a href={x.mapHref} rel="noreferrer" target="_blank" className="block underline">
          Otevřít mapu
        </a>
      </div>
    </Card>
  );
};
