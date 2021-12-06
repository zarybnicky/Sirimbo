import * as React from 'react';
import { Link } from 'react-router-dom';
import {
  Container, Button, Grid, Typography, Card, CardMedia, CardContent,
  CardActionArea, CardActions, makeStyles
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

export const ArticleList = ({ }) => {
  const classes = useStyles();
  return <Container maxWidth="lg">
    <Typography gutterBottom variant="h4" component="h2">Aktuálně</Typography>
    <Grid container spacing={3}>
      {items.map((x, i) => (
        <Grid item sm={6} md={3}>
          <Card key={i} elevation={3}>
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
          </Card>
        </Grid>
      ))}
    </Grid>
  </Container>;
};

const items = [
  {
    href: "/o-nas",
    img: "https://picsum.photos/360/240?random=1",
    header: "Přípravný kurz tanečního sportu",
    preview: "Otevíráme kurz pro mládež a dospělé v Olomouci a Přerově"
  },
  {
    href: "/o-nas",
    img: "https://picsum.photos/360/240?random=2",
    header: "Přípravný kurz tanečního sportu",
    preview: "Otevíráme kurz pro mládež a dospělé v Olomouci a Přerově"
  },
  {
    href: "/o-nas",
    img: "https://picsum.photos/360/240?random=3",
    header: "Přípravný kurz tanečního sportu",
    preview: "Otevíráme kurz pro mládež a dospělé v Olomouci a Přerově"
  },
  {
    href: "/o-nas",
    img: "https://picsum.photos/360/240?random=4",
    header: "Přípravný kurz tanečního sportu",
    preview: "Otevíráme kurz pro mládež a dospělé v Olomouci a Přerově"
  },
];
