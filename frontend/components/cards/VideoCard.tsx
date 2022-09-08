import * as React from 'react';
import { Link } from 'react-router-dom';
import { Grid, Paper, Typography, CardActionArea, makeStyles } from '@material-ui/core';
import { Video } from '../../data/use-videos';

import PlayIcon from '../../style/play_white.png';

const useStyles = makeStyles((theme) => ({
  img: {
    position: 'relative',
    '& img': {
      display: 'block',
      height: '100%',
      width: '100%',
      objectFit: 'cover',
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
    [theme.breakpoints.down('sm')]: {
      padding: '.5rem 1rem',
    },
  }
}));

export const VideoCard = ({ item: x }: { item: Video }) => {
  const classes = useStyles();
  return <Paper elevation={3}>
    <CardActionArea component={Link} to={x.href}>
      <Grid container>
        <Grid item sm={12} md={3} className={classes.img}>
          <img src={x.img} alt={x.name} />
        </Grid>
        <Grid item sm={12} md={9} className={classes.inner}>
          <Typography variant="h6" component="h2" className="header">{x.name}</Typography>
        </Grid>
      </Grid>
    </CardActionArea>
  </Paper>;
};
