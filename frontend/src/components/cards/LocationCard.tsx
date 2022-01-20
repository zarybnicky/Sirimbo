import * as React from 'react';
import { makeStyles, Typography, Paper, Grid } from '@material-ui/core';
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
      height: '150px',
      marginTop: '1rem'
    },
    '& .MuiTypography-body1': {
      color: theme.palette.secondary.main,
      display: 'block',
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

interface Location {
  img: string;
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
  const classes = useStyles();
  return <Paper elevation={3} className={classes.item}>
    <Grid container>
      <Grid item xs={12} sm={4} className="image">
        <img src={x.img} alt={x.name} />
      </Grid>
      <Grid item xs={12} sm="auto" className="body">
        <Typography variant="h5" component="h3" className="header">{x.name}</Typography>
        <Typography variant="body1">{x.address}</Typography>
        {x.href && <Typography variant="body1" component="a" href={x.href} target="_blank">{x.href}</Typography>}
        <Typography variant="body1" component="a" href={x.mapHref} target="_blank">
          Otevřít mapu
        </Typography>
        <Map name={x.name} {...x.map} />
      </Grid>
    </Grid>
  </Paper>;
};
