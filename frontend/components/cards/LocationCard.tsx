import * as React from 'react';
import { CellPlugin } from '@react-page/editor';
import { Typography, Paper, Grid } from '@mui/material';
import { defaultSlate } from '../SlateReadonly';
import { Map } from 'components/Map';

const CardMap = (props: {
  lat: number;
  lng: number;
  zoom: number;
  name: string;
}) => {
  return <Paper elevation={3}>
    <Map className="map" center={props} zoom={props.zoom} scrollWheelZoom={false}>
      {({ TileLayer, Marker, Popup }) => <>
        <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
        <Marker position={props}>
          <Popup>{props.name}</Popup>
        </Marker>
      </>}
    </Map>
  </Paper>;
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
  return <Paper elevation={3} sx={{
    margin: '2rem 0',
    '& .image': {
      position: 'relative',
    },
    '& .image img': {
      width: '100%',
      height: '100%',
      objectFit: 'cover',
    },
    '& .body': {
      padding: '2rem 2rem 1rem',
      flexGrow: 1,
      flexBasis: '1rem',
    },
    '& .header': {
      fontWeight: 'bold',
    },
    '& .map': {
      height: '150px',
      marginTop: '1rem'
    },
  }}>
    <Grid container>
      <Grid item xs={12} sm={4} className="image">
        <img src={x.image} alt={x.name} />
      </Grid>
      <Grid item xs={12} sm="auto" className="body text-gray-800">
        <Typography variant="h5" component="h3" className="header">{x.name}</Typography>
        <Typography variant="body1">{x.address}</Typography>
        {x.href && <Typography variant="body1" component="a" href={x.href} rel="noreferrer" target="_blank">{x.href}</Typography>}
        <Typography variant="body1" component="a" href={x.mapHref} rel="noreferrer" target="_blank">
          Otevřít mapu
        </Typography>
        <CardMap name={x.name} {...x.map} />
      </Grid>
    </Grid>
  </Paper>;
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
