import * as React from 'react';
import Link from 'next/link';
import { Grid, Paper, Typography, CardActionArea, useTheme } from '@mui/material';
import { Video } from 'lib/data/use-videos';

import PlayIcon from 'public/style/play_white.png';

export const VideoCard = ({ item: x }: { item: Video }) => {
  const theme = useTheme();
  return <Paper elevation={3}>
    <CardActionArea component={Link} href={x.href}>
      <Grid container>
        <Grid item sm={12} md={3} sx={{
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
        }}>
          <img src={x.img} alt={x.name} />
        </Grid>

        <Grid item sm={12} md={9} sx={{
          flexGrow: 1,
          display: 'flex',
          justifyContent: 'stretch',
          alignItems: 'center',
          textDecoration: 'underline',
          paddingLeft: '2rem',
          [theme.breakpoints.down('sm')]: {
            padding: '.5rem 1rem',
          },
        }}>
          <Typography variant="h6" component="h2" className="header">{x.name}</Typography>
        </Grid>
      </Grid>
    </CardActionArea>
  </Paper>;
};
