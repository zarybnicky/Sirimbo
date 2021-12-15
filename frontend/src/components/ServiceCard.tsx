import * as React from 'react';
import { makeStyles, Grid, Paper, Typography } from '@material-ui/core';

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

interface Service {
  img: string;
  header: string;
  text: string;
}

export const ServiceCard = ({ item: x }: { item: Service }) => {
  const classes = useStyles();
  return <Paper elevation={3} className={classes.item}>
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
  </Paper>;
};
