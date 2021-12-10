import * as React from 'react';
import { Link } from 'react-router-dom';
import { makeStyles, Typography } from '@material-ui/core';

const useStyles = makeStyles(() => ({
  container: {
    position: 'relative',
    width: '100%',
    height: '250px',
    overflow: 'hidden',
    '& img': {
      overflow: 'hidden',
      width: '100%',
      height: '100%',
      maxHeight: '600px',
      objectFit: 'cover',
      transition: 'transform .3s',
    },
    '&:hover img': {
      transform: 'scale(1.1)',
    },
    '& .overlay': {
      zIndex: 10,
      position: 'absolute',
      left: 0,
      right: 0,
      bottom: '15px',
      background: 'rgba(255, 255, 255, .8)',
      display: 'flex',
      flexDirection: 'column',
      textDecoration: 'none',
      color: 'black',
      padding: '.25rem .75rem',
    }
  },
}));

interface GalleryItem {
  img: string;
  href: string;
  name: string;
  date: string;
}

export const GalleryCard = ({ item: x }: { item: GalleryItem }) => {
  const classes = useStyles();

  return <div className={classes.container}>
    <Link className="overlay" to={x.href}>
      <Typography variant="body1">{x.name}</Typography>
      <Typography variant="body1">{x.date}</Typography>
    </Link>
    <img src={x.img} alt={`${x.name} - ${x.date}`} /></div>;
}
