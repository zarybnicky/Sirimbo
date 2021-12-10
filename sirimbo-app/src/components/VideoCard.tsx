import * as React from 'react';
import { Link } from 'react-router-dom';
import { Grid, Paper, Typography, CardActionArea, makeStyles } from '@material-ui/core';

import PlayIcon from '../style/play_white.png';

const useStyles = makeStyles(() => ({
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

interface Video {
  img: string;
  href: string;
  name: string;
}

export const VideoCard = ({ item: x }: { item: Video }) => {
  const classes = useStyles();
  return <Paper elevation={3}>
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
  </Paper>;
};
