import * as React from 'react';
import Link from 'next/link';
import {
  Button, Typography, Card, CardMedia, CardContent, CardActionArea, CardActions, makeStyles
} from '@mui/material';
import { Article } from 'lib/data/use-articles';

const useStyles = makeStyles((theme) => ({
  header: {
    color: theme.palette.primary.main,
    fontWeight: 'bold',
    position: 'relative',
    marginBottom: '15px',
    '&::after': {
      content: '""',
      position: 'absolute',
      bottom: '-7px',
      left: 0,
      right: '50%',
      height: '5px',
      backgroundColor: theme.palette.primary.main,
    },
  },
  buttons: {
    justifyContent: 'center',
    paddingBottom: '2px',
  },
}));

export const ArticleCard = ({ item: x }: { item: Article }) => {
  const classes = useStyles();
  return <Card elevation={3} style={{ flexGrow: 1, display: 'flex', flexDirection: 'column' }}>
    <CardActionArea LinkComponent={Link} href={x.href} style={{ flexGrow: 1 }}>
      <CardMedia component="img" height={240} image={x.img} title={x.header} />
      <CardContent>
        <Typography gutterBottom variant="subtitle1" component="h3" className={classes.header}>{x.header}</Typography>
        <Typography variant="body2" color="textSecondary" component="p">{x.preview}</Typography>
      </CardContent>
    </CardActionArea>
    <CardActions className={classes.buttons}>
      <Button size="large" variant="contained" color="primary" LinkComponent={Link} href={x.href}>
        Více zde ᐳ
      </Button>
    </CardActions>
  </Card>;
};
