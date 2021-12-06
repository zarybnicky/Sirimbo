import * as React from 'react';
import { Link } from 'react-router-dom';
import { Container, Grid, Paper, Typography, CardActionArea, makeStyles } from '@material-ui/core';

import PlayIcon from '../style/play_white.png';

const useStyles = makeStyles((theme) => ({
  img: {
    height: '96px',
    position: 'relative',
    '& img': {
      display: 'block',
      height: '100%',
    },
    '&::after': {
      content: '""',
      display: 'block',
      position: 'absolute',
      top: 0,
      left: 0,
      width: '100%',
      height: '100%',
      background: `url(${PlayIcon}) no-repeat center/35%`,
    },
  },
  inner: {
    flexGrow: 1,
    display: 'flex',
    justifyContent: 'stretch',
    alignItems: 'center',
    textDecoration: 'underline',
    paddingLeft: '2rem',
  }
}));

export const VideoList = ({ }) => {
  const classes = useStyles();
  return <Container maxWidth="lg" style={{ margin: '3rem auto' }}>
    <Typography gutterBottom variant="h4" component="h2">Videa</Typography>
    <Grid container spacing={3}>
      {items.map((x, i) => (
        <Grid item sm={6}>
          <Paper key={i} elevation={3}>
            <CardActionArea component={Link} to={x.href}>
              <Grid container>
                <Grid item className={classes.img}>
                  <img src={x.img} alt={x.name} />
                </Grid>
                <Grid item className={classes.inner}>
                  <Typography variant="h5" component="h2" className="header">{x.name}</Typography>
                </Grid>
              </Grid>
            </CardActionArea>
          </Paper>
        </Grid>
      ))}
    </Grid>
  </Container>;
};

const items = [
  {
    href: "/o-nas",
    img: "https://picsum.photos/360/240?random=1",
    name: "Přípravný kurz tanečního sportu",
  },
  {
    href: "/o-nas",
    img: "https://picsum.photos/360/240?random=2",
    name: "Přípravný kurz tanečního sportu",
    preview: "Otevíráme kurz pro mládež a dospělé v Olomouci a Přerově"
  },
  {
    href: "/o-nas",
    img: "https://picsum.photos/360/240?random=3",
    name: "Přípravný kurz tanečního sportu",
    preview: "Otevíráme kurz pro mládež a dospělé v Olomouci a Přerově"
  },
  {
    href: "/o-nas",
    img: "https://picsum.photos/360/240?random=4",
    name: "Přípravný kurz tanečního sportu",
    preview: "Otevíráme kurz pro mládež a dospělé v Olomouci a Přerově"
  },
];
