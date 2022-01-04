import * as React from 'react';
import { Container, Grid, Paper, Typography, makeStyles } from '@material-ui/core';
import { MapContainer, TileLayer, Marker, Popup } from 'react-leaflet';
import clsx from 'clsx';
import { LatLngTuple } from 'leaflet';

import CstsLogo from '../../static/images/csts-logo.svg';
import OlomoucLogo from '../../static/style/logo-olomouc.jpg';
import KrajLogo from '../../static/style/logo-kraj.png';
import { SocialButtons } from './SocialButtons';

const useStyles = makeStyles((theme) => ({
  section: {
    padding: '3rem 0 5rem',
    color: theme.palette.secondary.contrastText,
    backgroundColor: theme.palette.secondary.main
  },
  h2: {
    fontSize: '2rem',
    fontWeight: 900,
  },
  h3: {
    color: theme.palette.primary.main,
    fontWeight: 700,
    fontSize: '1.2rem',
    marginTop: '1rem',
  },
  h4: {
    fontSize: '1.2rem',
    fontWeight: 900,
    marginTop: '.5rem',
  },
  gutter: {
    marginTop: '1rem',
  },
  map: {
    height: '200px'
  },
  iconBox: {
    margin: '1rem 0',
    textAlign: 'right',
  },
  sponsorsBox: {
    padding: '1rem',
    textAlign: 'center',
  },
  sponsorsHead: {
    color: theme.palette.primary.main,
    fontSize: '1.2rem',
    fontWeight: 900,
    textAlign: 'left',
    marginBottom: '1rem',
  }
}));

const ContactText = () => {
  const classes = useStyles();
  return <React.Fragment>
    <Typography variant="h2" className={classes.h2}>Kontakt:</Typography>

    <Typography variant="h3" className={clsx(classes.h3, classes.gutter)}>Taneční klub</Typography>
    <Typography variant="h4" className={classes.h4}>Taneční klub Olymp Olomouc</Typography>
    <Typography>
      Jiráskova 25, 779 00 Olomouc<br />
      IČO: 68347286<br />
      tkolymp@tkolymp.cz
    </Typography>

    <br />
    <br />

    <Typography variant="h3" className={classes.h3}>Taneční sály</Typography>
    <Typography variant="h4" className={classes.h4}>Taneční centrum při FZŠ Holečkova</Typography>
    <Typography>
      Holečkova 10, 779 00 Olomouc<br />
      (vchod brankou u zastávky Povel, škola)
    </Typography>

    <br />
    <Typography variant="h4" className={classes.h4}>Tělocvična Slovanského gymnázia</Typography>
    <Typography>
      Jiřího z Poděbrad 13, 779 00 Olomouc<br />
      (vchod bránou z ulice U reálky)
    </Typography>
  </React.Fragment>;
}

const Map = () => {
  const classes = useStyles();
  const position = [49.58727525, 17.25661055] as LatLngTuple;
  const holeckova = [49.57963, 17.2495939] as LatLngTuple;
  const slovan = [49.59490, 17.26340] as LatLngTuple;

  return <MapContainer className={classes.map} center={position} zoom={12} scrollWheelZoom={false}>
    <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
    <Marker position={holeckova}>
      <Popup>Taneční centrum při FZŠ Holečkova</Popup>
    </Marker>
    <Marker position={slovan}>
      <Popup>Tělocvična Slovanského gymnázia</Popup>
    </Marker>
  </MapContainer>;
};

export const Footer = ({ }) => {
  const classes = useStyles();

  return <div className={classes.section}>
    <Container maxWidth="lg">
      <Grid container spacing={3}>
        <Grid item xs><ContactText /></Grid>
        <Grid item xs>
          <Map />
          <SocialButtons className={classes.iconBox} variant="large" />

          <Paper className={classes.sponsorsBox}>
            <Typography variant="h2" className={classes.sponsorsHead}>Podporují nás</Typography>
            <Grid container spacing={3} alignItems="center">
              <Grid item xs={4}>
                <CstsLogo height="100" width="100" />
              </Grid>
              <Grid item xs={4}>
                <img alt="Město Olomouc" style={{ width: '100%', height: 'auto' }} src={OlomoucLogo} />
              </Grid>
              <Grid item xs={4}>
                <img alt="Olomoucký kraj" style={{ width: '100%', height: 'auto' }} src={KrajLogo} />
              </Grid>
            </Grid>
          </Paper>
        </Grid>
      </Grid>
      <Typography variant="body1" component="div" style={{ marginTop: '1rem' }}>
        Realizace: Jakub Zárybnický
      </Typography>
      <Typography variant="body1" component="div">
        © 2022 Taneční klub Olymp Olomouc, z. s.
      </Typography>
    </Container>
  </div>;
};
