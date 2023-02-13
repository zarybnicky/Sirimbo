import * as React from 'react';
import { CellPlugin } from '@react-page/editor';
import { defaultSlate } from '../Slate';
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
  return <Card>
    <Map className={classNames("map", props.className)} center={props} zoom={props.zoom} scrollWheelZoom={false}>
      {({ TileLayer, Marker, Popup }) => <>
        <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
        <Marker position={props}>
          <Popup>{props.name}</Popup>
        </Marker>
      </>}
    </Map>
  </Card>;
};

type Location = {
  image: string;
  name: string;
  address: string;
  href: string | null;
  mapHref: string;
  map: {
    lat: number;
    lng: number;
    zoom: number;
  };
}

export const LocationCard = ({ item: x }: { item: Location; }) => {
  return <Card className="my-8 grid md:grid-cols-[1fr_2fr]">
    <div className="relative -order-1 md:order-1">
      <img className="w-full h-full object-cover" src={x.image} alt={x.name} />
    </div>
    <div className="p-8 pb-4 grow basis-4 text-gray-800">
      <h5 className="text-lg font-bold mb-2">{x.name}</h5>
      <p>{x.address}</p>
      {x.href && <a href={x.href} rel="noreferrer" target="_blank">{x.href}</a>}
      <a href={x.mapHref} rel="noreferrer" target="_blank">
        Otevřít mapu
      </a>
      <CardMap className="h-[150px] mt-4" name={x.name} {...x.map} />
    </div>
  </Card>;
};


export const LocationCardPlugin: CellPlugin<Location> = {
  Renderer: ({ data }) => <LocationCard item={data} />,

  id: 'app-location-card-plugin',
  title: 'LocationCard',
  version: 1,
  createInitialData: () => ({
    image: '',
    name: 'Taneční centrum při FZŠ Holečkova',
    address: 'Holečkova 10, 779 00, Olomouc (vchod brankou u zastávy Povel - škola)',
    href: 'https://www.zsholeckova.cz/',
    mapHref: 'https://goo.gl/maps/swv3trZB2uvjcQfR6',
    map: {
      lat: 49.57963,
      lng: 17.2495939,
      zoom: 12,
    },
  }),
  createInitialChildren: () => [[{ plugin: defaultSlate }]],
  controls: {
    type: 'autoform',
    schema: {
      required: [],
      properties: {
        image: { type: 'string' },
        name: { type: 'string' },
        address: { type: 'string' },
        href: { type: 'string' },
        mapHref: { type: 'string' },
        map: {
          type: 'object',
          properties: {
            lat: { type: 'number' },
            lng: { type: 'number' },
            zoom: { type: 'integer' },
          },
        },
      },
    },
  },
};
