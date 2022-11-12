import * as React from 'react';
import { Typography, Card, CardMedia, CardContent, CardActionArea, CardActions } from '@mui/material';
import { Article } from 'lib/data/use-articles';
import { NextLinkComposed } from 'components/Link';
import Link from 'next/link';

export const ArticleCard = ({ item: x }: { item: Article }) => {
  return <Card elevation={3} sx={{
    flexGrow: 1,
    display: 'flex',
    flexDirection: 'column',
  }}>
    <CardActionArea component={NextLinkComposed} href={x.href} style={{ flexGrow: 1 }}>
      <CardMedia component="img" height={240} image={x.img} title={x.header} />
      <CardContent>
        <Typography className="mb-4 relative text-red-500 after:bg-red-500 font-bold" gutterBottom variant="subtitle1" component="h3" sx={{
          '&::after': {
            content: '""',
            position: 'absolute',
            bottom: '-7px',
            left: 0,
            right: '50%',
            height: '5px',
          },
        }}>{x.header}</Typography>

        <Typography variant="body2" color="textSecondary" component="p">{x.preview}</Typography>
      </CardContent>
    </CardActionArea>

    <CardActions className="justify-center pb-1">
      <Link href={x.href} passHref>
        <a className="button button-red button-lg">Více zde ᐳ</a>
      </Link>
    </CardActions>
  </Card>;
};
