import * as React from 'react';
import { Container, Grid, Paper, Typography, Box, useTheme } from '@mui/material';
import { SocialButtons } from './SocialButtons';
import { Map } from './Map';

import CstsLogo from 'public/images/csts-logo.svg';
import OlomoucLogo from 'public/style/logo-olomouc.jpg';
import KrajLogo from 'public/style/logo-kraj.png';

const ContactText = () => {
  const theme = useTheme();

  return <>
    <Typography variant="h2" sx={{
      fontSize: '2rem',
      fontWeight: 900,
    }}>Kontakt:</Typography>

    <Typography variant="h3" sx={{
      color: theme.palette.primary.main,
      fontWeight: 700,
      fontSize: '1.2rem',
      marginTop: '1rem',
    }}>Taneční klub</Typography>

    <Typography variant="h4" sx={{
      fontSize: '1.2rem',
      fontWeight: 900,
      marginTop: '1rem',
    }}>Taneční klub Olymp Olomouc</Typography>

    <Typography>
      Jiráskova 25, 779 00 Olomouc<br />
      IČO: 68347286<br />
      miroslav.hyza@tkolymp.cz
    </Typography>

    <br />
    <br />

    <Typography variant="h3" sx={{
      color: theme.palette.primary.main,
      fontWeight: 700,
      fontSize: '1.2rem',
      marginTop: '1rem',
    }}>Taneční sály</Typography>

    <Typography variant="h4" sx={{
      fontSize: '1.2rem',
      fontWeight: 900,
      marginTop: '.5rem',
    }}>Taneční centrum při FZŠ Holečkova</Typography>

    <Typography>
      Holečkova 10, 779 00 Olomouc<br />
      (vchod brankou u zastávky Povel, škola)
    </Typography>

    <br />
    <Typography variant="h4" sx={{
      fontSize: '1.2rem',
      fontWeight: 900,
      marginTop: '.5rem',
    }}>Tělocvična Slovanského gymnázia</Typography>
    <Typography>
      Jiřího z Poděbrad 13, 779 00 Olomouc<br />
      (vchod bránou z ulice U reálky)
    </Typography>
  </>;
}

export const FooterMap = ({ height = '200px' }) => {
  const position = [49.58727525, 17.25661055] as [number, number];
  const holeckova = [49.57963, 17.2495939] as [number, number];
  const slovan = [49.59490, 17.26340] as [number, number];

  return <Map center={position} zoom={12} scrollWheelZoom={false} style={{ height }}>
    {({ TileLayer, Marker, Popup }) => <>
      <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
      <Marker position={holeckova}>
        <Popup>Taneční centrum při FZŠ Holečkova</Popup>
      </Marker>
      <Marker position={slovan}>
        <Popup>Tělocvična Slovanského gymnázia</Popup>
      </Marker>
    </>}
  </Map>;
};

export const Footer: React.FC = () => {
  const theme = useTheme();
  return <Box sx={{
    padding: '3rem 0 5rem',
    color: theme.palette.secondary.contrastText,
    backgroundColor: theme.palette.secondary.main
  }}>
    <Container maxWidth="lg">
      <Grid container spacing={1.5}>
        <Grid item xs><ContactText /></Grid>
        <Grid item xs>
          <FooterMap />
          <SocialButtons variant="large" sx={{
            margin: '1rem 0',
            textAlign: 'right',
          }} />

          <Paper sx={{
            padding: '1rem',
            textAlign: 'center',
          }}>
            <Typography variant="h2" sx={{
              color: theme.palette.primary.main,
              fontSize: '1.2rem',
              fontWeight: 900,
              textAlign: 'left',
              marginBottom: '1rem',
            }}>Podporují nás</Typography>

            <Grid container spacing={1.5} alignItems="center">
              <Grid item xs={4}>
                <img alt="ČSTS" style={{ width: '100%', height: 'auto' }} src={CstsLogo.src} />
              </Grid>
              <Grid item xs={4}>
                <img alt="Město Olomouc" style={{ width: '100%', height: 'auto' }} src={OlomoucLogo.src} />
              </Grid>
              <Grid item xs={4}>
                <img alt="Olomoucký kraj" style={{ width: '100%', height: 'auto' }} src={KrajLogo.src} />
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
  </Box>;
};
