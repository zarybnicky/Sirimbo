import * as React from 'react';
import { Container, Grid, Typography, makeStyles } from '@material-ui/core';

const useStyles = makeStyles((theme) => ({
  container: {
    margin: '3rem auto',
  },
}));

// https://morioh.com/p/7c097570ecd9
export const HighlightList = ({ }) => {
  const classes = useStyles();
  return <Container maxWidth="xl" className={classes.container}>
    <Grid container>
      {items.map((x, i) => (
        <Grid item sm={6} md={3} key={i}>
          <img src={x.img} />
          <Typography variant="h4" component="div">{x.text}</Typography>
        </Grid>
      ))}
    </Grid>
  </Container>;
}

const items = [
  { img: "https://picsum.photos/360/240?random=1", text: "Co?" },
  { img: "https://picsum.photos/360/240?random=2", text: "Tohle?" },
  { img: "https://picsum.photos/360/240?random=3", text: "Má?" },
  { img: "https://picsum.photos/360/240?random=4", text: "Být?" },
];
