import * as React from 'react';
import { Link } from 'react-router-dom';
import { makeStyles, Typography } from '@material-ui/core';
import Carousel from 'react-material-ui-carousel'
import { useHeroData } from '../data';

const useStyles = makeStyles((theme) => ({
  indicator: {
    fill: theme.palette.grey[600],
    marginBottom: '2rem',
  },
  activeIndicator: {
    fill: theme.palette.primary.main,
  },
  container: {
    position: 'relative',
    width: '100%',
    overflow: 'hidden',
    '& img': {
      width: '100%',
      height: '60vh',
      maxHeight: '600px',
      objectFit: 'cover',
      transition: 'transform .3s',
    },
    '& .overlay:hover + img': {
      transform: 'scale(1.1)',
    },
    '& .overlay': {
      zIndex: 10,
      position: 'absolute',
      left: 0,
      right: 0,
      bottom: 0,
      background: 'linear-gradient(90deg, rgba(216,28,58,.8) 0%, rgba(0,0,0,0.8) 50%, rgba(216,28,58,.8) 100%)',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      textDecoration: 'none',
      color: 'white',
      padding: '1rem',
    },
    '& .overlay:hover': {
      textDecoration: 'underline',
    }
  },
}));

export const Hero = ({ }) => {
  const classes = useStyles();
  const items = useHeroData();

  return <Carousel
    timeout={400}
    IndicatorIcon={<svg width="50" height="9" viewBox="5 0 50 9"><rect width="40" height="9" /></svg>}
    indicatorIconButtonProps={{ className: classes.indicator }}
    activeIndicatorIconButtonProps={{ className: classes.activeIndicator }}
  >
    {items.map((x, i) => (
      <div key={i} className={classes.container}>
        <Link className="overlay" to={x.href}>
          <Typography variant="h5">{x.text}</Typography>
        </Link>
        <img src={x.img} alt={x.text} />
      </div>
    ))}
  </Carousel>;
}