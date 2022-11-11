import * as React from 'react';
import { CellPlugin } from '@react-page/editor';
import { Typography, Grid } from '@mui/material';
import { defaultSlate } from '../SlateReadonly';
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
  return <Card className="my-8">
    <Grid container>
      <Grid item xs={12} sm={4} className="relative">
        <img className="w-full h-full object-cove" src={x.image} alt={x.name} />
      </Grid>
      <Grid item xs={12} sm="auto" className="p-8 pb-4 grow basis-4 text-gray-800">
        <Typography variant="h5" component="h3" className="font-bold">{x.name}</Typography>
        <Typography variant="body1">{x.address}</Typography>
        {x.href && <Typography variant="body1" component="a" href={x.href} rel="noreferrer" target="_blank">{x.href}</Typography>}
        <Typography variant="body1" component="a" href={x.mapHref} rel="noreferrer" target="_blank">
          Otevřít mapu
        </Typography>
        <CardMap className="h-[150px] mt-4" name={x.name} {...x.map} />
      </Grid>
    </Grid>
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
