import * as React from 'react';
import { Link } from 'react-router-dom';
import { makeStyles, Container, Grid, Paper, Typography } from '@material-ui/core';

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
    '& .gutter': {
      minHeight: '1rem',
      minWidth: '1rem',
      background: theme.palette.secondary.main,
    },
    '&:nth-of-type(even) .gutter': {
      background: theme.palette.primary.main,
    },
    '& .body': {
      padding: '3rem 2rem',
      flexGrow: 1,
      flexBasis: '1rem',
    },
    '& .header': {
      color: theme.palette.secondary.main,
      fontWeight: 'bold',
      marginBottom: '1rem',
    },
    '&:nth-of-type(even) .header': {
      color: theme.palette.primary.main,
    },
  }
}));

// Red/black separator + header text, spacing
export const ServiceList = ({ }) => {
  const classes = useStyles();
  return <Container maxWidth="lg">
    {items.map((x, i) => (
      <Paper key={i} elevation={3} className={classes.item}>
        <Grid container>
          <Grid item xs={12} sm={4} className="image">
            <img src={x.img} alt={x.header} />
          </Grid>
          <Grid item xs={12} sm="auto" className="gutter" />
          <Grid item xs={12} sm="auto" className="body">
            <Typography variant="h5" component="h2" className="header">{x.header}</Typography>
            <Typography variant="body1">{x.text}</Typography>
          </Grid>
        </Grid>
      </Paper>
    ))}
  </Container>;
};

const items = [
  {
    img: "https://picsum.photos/300/100?random=1",
    href: null,
    header: "Přípravka tanečního sportu",
    text: "První kroky do světa tanečního sportu pro děti od 5 do 10 let. Všeobecná taneční průprava a základy tanečních kroků pro budoucí hvězdy."
  },
  {
    img: "https://picsum.photos/300/100?random=2",
    href: null,
    header: "Základy tanečního sportu",
    text: "Tréninkové programy pro začínající a mírně pokročilé tanečníky ve věkových skupinách juniorů (12-15 let), mládež a dospělí (16+ let)."
  },
  {
    img: "https://picsum.photos/300/100?random=3",
    href: null,
    header: "Výkonnostní sport",
    text: "Tréninkové programy pro soutěžní tanečníky ve všech věkových skupinách a výkonnostních třídách uzpůsobené podle potřeb v jednotlivých výkonnostních stupních."
  },
  {
    img: "https://picsum.photos/300/100?random=4",
    href: null,
    header: "Sportovní centrum mládeže",
    text: "Tréninkový program pro vrcholové sportovce, reprezentanty ČR se špičkovými českými trenéry, speciální kondiční přípravou a moderními metodami sportovního tréninku. Jsme jediným klubem v Olomouckém kraji se statutem Sprtovního centra mládeže dle MŠMT."
  },
];
