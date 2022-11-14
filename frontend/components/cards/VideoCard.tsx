import * as React from 'react';
import { Grid, CardActionArea } from '@mui/material';
import { Video } from 'lib/data/use-videos';
import { NextLinkComposed } from 'components/Link';
import PlayIcon from 'public/style/play_white.png';
import { Card } from 'components/Card';

export const VideoCard = ({ item: x }: { item: Video }) => {
  return <Card>
    <CardActionArea component={NextLinkComposed} href={x.href}>
      <Grid container>
        <Grid item sm={12} md={3} className="relative" sx={{
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
          <img src={x.img} alt={x.name} className="block w-full h-full object-cover" />
        </Grid>

        <Grid item sm={12} md={9} className="flex grow justify-stretch items-center px-4 py-2 md:pl-8 underline">
          <h6>{x.name}</h6>
        </Grid>
      </Grid>
    </CardActionArea>
  </Card>;
};
