import * as React from 'react';
import { Link } from 'react-router-dom';
import {
  Button, Typography, Card, CardMedia, CardContent, CardActionArea, CardActions, makeStyles
} from '@material-ui/core';

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

interface Article {
  img: string;
  href: string;
  header: string;
  preview: string;
}

export const ArticleCard = ({ item: x }: { item: Article; }) => {
  const classes = useStyles();
  return <Card elevation={3}>
    <CardActionArea component={Link} to={x.href}>
      <CardMedia component="img" height={240} image={x.img} title={x.header} />
      <CardContent>
        <Typography gutterBottom variant="subtitle1" component="h3" className={classes.header}>{x.header}</Typography>
        <Typography variant="body2" color="textSecondary" component="p">{x.preview}</Typography>
      </CardContent>
    </CardActionArea>
    <CardActions className={classes.buttons}>
      <Button size="large" variant="contained" color="primary" component={Link} to={x.href}>
        Více zde ᐳ
      </Button>
    </CardActions>
  </Card>;
};
