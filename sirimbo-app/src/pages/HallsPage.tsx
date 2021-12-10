import * as React from 'react';
import { makeStyles, Container, Typography, Paper, Grid } from '@material-ui/core';
import { MapContainer, TileLayer, Marker, Popup } from 'react-leaflet';

const useStyles = makeStyles((theme) => ({
  item: {
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
      marginTop: '1rem',
      height: '150px',
    },
  }
}));

const Map = (props: {
  lat: number;
  lng: number;
  zoom: number;
  name: string;
}) => {
  return <Paper elevation={3}>
    <MapContainer className="map" center={props} zoom={props.zoom} scrollWheelZoom={false}>
      <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
      <Marker position={props}><Popup>{props.name}</Popup></Marker>
    </MapContainer>
  </Paper>;
};

export const HallsPage = ({ }) => {
  const classes = useStyles();
  return <Container maxWidth="md">
    {items.map((x, i) => (
      <Paper key={i} elevation={3} className={classes.item}>
        <Grid container>
          <Grid item xs={12} sm={4} className="image">
            <img src={x.img} alt={x.name} />
          </Grid>
          <Grid item xs={12} sm="auto" className="body">
            <Typography variant="h5" component="h2" className="header">{x.name}</Typography>
            <Typography variant="body1">{x.address}</Typography>
            {x.href && <Typography variant="body1" component="a" href={x.href} target="_blank">{x.href}</Typography>}
            <Map name={x.name} {...x.map} />
          </Grid>
        </Grid>
      </Paper>
    ))}
  </Container>;
};

const items = [
  {
    img: 'https://picsum.photos/360/240?random=1',
    name: 'Taneční centrum při FZŠ Holečkova',
    address: 'Holečkova 10, 779 00, Olomouc (vchod brankou u zastávy Povel - škola)',
    href: 'https://www.zsholeckova.cz/',
    map: {
      lat: 49.57963,
      lng: 17.2495939,
      zoom: 12,
    },
  },
  {
    img: 'https://picsum.photos/360/240?random=2',
    name: 'Tělocvična Slovanského gymnázia',
    address: 'Jiřího z Poděbrad 13, 779 00 Olomouc (vchod brankou z ulice U reálky)',
    href: 'https://www.sgo.cz/',
    map: {
      lat: 49.59490,
      lng: 17.26340,
      zoom: 12,
    },
  },
  {
    img: 'https://picsum.photos/360/240?random=3',
    name: 'T.J. Sokol Přerov',
    address: 'Brabansko 2, 750 02 Přerov',
    href: 'https://www.sokolprerov.cz/',
    map: {
      lat: 49.4574331,
      lng: 17.4480036,
      zoom: 12,
    },
  },
  {
    img: 'https://picsum.photos/360/240?random=4',
    name: 'Taneční sál Gala',
    address: 'Západní 1, 796 04 Prostějov-Krasice (vchod vedle podnikové prodejny Gala)',
    href: null,
    map: {
      lat: 49.4681836,
      lng: 17.0837344,
      zoom: 12,
    },
  },
];
