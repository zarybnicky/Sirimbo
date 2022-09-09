import * as React from 'react';
import Link from 'next/link';
import { makeStyles, Typography } from '@mui/material';
import Carousel from 'react-mui-carousel'
import { useArticles } from 'lib/data/use-articles';

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
    height: '60vh',
    maxHeight: '600px',
    '& img': {
      height: '100%',
      width: '100%',
      objectFit: 'cover',
      objectPosition: '50% 30%',
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
      textAlign: 'center',
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
  let { articles } = useArticles(3, 0);
  articles = articles.length > 0 ? articles : [{
    href: '#',
    img: '',
    imgThumb: '',
    header: '',
    preview: '',
  }];

  return <Carousel
    timeout={400}
    IndicatorIcon={<svg width="50" height="9" viewBox="5 0 50 9"><rect width="40" height="9" /></svg>}
    indicatorIconButtonProps={{ className: classes.indicator }}
    activeIndicatorIconButtonProps={{ className: classes.activeIndicator }}
  >
    {articles.map((x, i) => (
      <div key={i} className={classes.container}>
        <Link href={x.href} passHref>
          <a className="overlay">
            <Typography variant="h5">{x.header}</Typography>
          </a>
        </Link>
        <img src={x.img} alt={x.header} />
      </div>
    ))}
  </Carousel>;
}
