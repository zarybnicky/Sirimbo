import * as React from 'react';
import { Grid, Paper, Typography, CardActionArea } from '@mui/material';
import { Video } from 'lib/data/use-videos';

import PlayIcon from 'public/style/play_white.png';
import { NextLinkComposed } from 'components/Link';

export const VideoCard = ({ item: x }: { item: Video }) => {
  return <Paper elevation={3}>
    <CardActionArea component={NextLinkComposed} href={x.href}>
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

        <Grid item sm={12} md={9} className="flex grow justify-stretch items-center px-4 py-2 md:pl-8 underline">
          <Typography variant="h6" component="h2" className="header">{x.name}</Typography>
        </Grid>
      </Grid>
    </CardActionArea>
  </Paper>;
};
