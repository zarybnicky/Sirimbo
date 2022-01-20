import * as React from 'react';
import { Link } from 'react-router-dom';
import { makeStyles, Typography } from '@material-ui/core';

const useStyles = makeStyles((theme) => ({
  container: {
    display: 'block',
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
      display: 'flex',
      flexDirection: 'column',
      position: 'absolute',
      left: 0,
      right: 0,
      bottom: '15px',
      padding: '.25rem .75rem',
      background: 'rgba(255, 255, 255, .9)',
      color: theme.palette.secondary.main,
      textDecoration: 'none',
      zIndex: 10,
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

  return <Link className={classes.container} to={x.href}>
    <div className="overlay">
      <Typography variant="body1">{x.name}</Typography>
      <Typography variant="body1">{x.date}</Typography>
    </div>
    <img src={x.img} alt={`${x.name} - ${x.date}`} />
  </Link>;
}
