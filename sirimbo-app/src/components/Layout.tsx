import * as React from 'react';
import { Link } from 'react-router-dom';
import { AppBar, Container, Paper, Button, Toolbar, Typography, Card, CardActionArea, CardMedia, CardContent, CardActions } from '@material-ui/core';
import Carousel from 'react-material-ui-carousel'
import { makeStyles } from '@material-ui/core/styles';
import { gql } from 'graphql-tag';

export const SingleParameter = gql(`
query SingleParameter($name: String!) {
  parameters_by_pk(pa_name: $name) {
    pa_value
  }
}`);
/* const { data } = useQuery(SingleParameter, { variables: { name: 'menu' } });
 * const menu = (!data?.parameters_by_pk?.pa_value) ? [] : JSON.parse(data?.parameters_by_pk?.pa_value) as Menu[];
 * const menuBar = menu.map((x) => <div key={x.text}>{x.type}: {x.text}</div>); */
/* type Menu = { type: 'link'; text: string; url: string; }
 *           | { type: 'menu'; text: string; children: Menu[]; }; */

const useStyles = makeStyles((theme) => ({
  navlinks: {
    justifyContent: 'space-between',
    display: "flex",
  },
  logo: {
    color: theme.palette.primary.main,
    flexGrow: 1,
    cursor: "pointer",
  },
  link: {
    textDecoration: "none",
    color: "white",
    fontSize: "20px",
    "&:hover": {
      color: "yellow",
      borderBottom: "1px solid white",
    },
  },
}));

export const AppHeader = () => {
  const classes = useStyles();

  return <AppBar position="static" color="secondary">
    <Toolbar>
      <Container maxWidth="lg" className={classes.navlinks}>
        <Typography variant="h4" className={classes.logo}>
          Navbar
        </Typography>
        <Link to="/" className={classes.link}>
          Home
        </Link>
        <Link to="/about" className={classes.link}>
          About
        </Link>
        <Link to="/contact" className={classes.link}>
          Contact
        </Link>
        <Link to="/faq" className={classes.link}>
          FAQ
        </Link>
        <div>
          FB YT LIn
        </div>
        <div>
          Icon
          <div>Přihlásit</div>
        </div>
      </Container>
    </Toolbar>
  </AppBar>;
};

export const AppFooter = () => <div>
  Kontakt

  Taneční klub Olymp Olomouc
  Jiráskova 25, 779 00 Olomouc
  IČO: 68347286
  tkolymp@tkolymp.cz

  Taneční sály
  Taneční centrum při FZŠ Holečkova
  Holečkova 10, 779 00 Olomouc
  (vchod brankou u zastávky Povel, škola)

  Tělocvična Slovanského gymnázia
  Jiřího z Poděbrad 13, 779 00 Olomouc
  (vchod bránou z ulice U reálky)
</div>;

export const HomePage = () => <React.Fragment>
  <HeroArticles />
  <Offerings />
  <CallToAction />
  <HighlightList />
  <ArticleList />
  <VideoList />
</React.Fragment>;

const ArticleList = () => {
  return <Container maxWidth="lg">
    <Typography variant="h2">Aktuálně</Typography>
    <Card>
      <CardActionArea>
        <CardMedia
          image="/static/images/cards/contemplative-reptile.jpg"
          title="Contemplative Reptile"
        />
        <CardContent>
          <Typography gutterBottom variant="h5" component="h2">
            Lizard
          </Typography>
          <Typography variant="body2" color="textSecondary" component="p">
            Lizards are a widespread group of squamate reptiles, with over 6,000 species, ranging
            across all continents except Antarctica
          </Typography>
        </CardContent>
      </CardActionArea>
      <CardActions>
        <Button size="small" color="primary">
          Share
        </Button>
        <Button size="small" color="primary">
          Learn More
        </Button>
      </CardActions>
    </Card>
  </Container>;
};

const VideoList = () => {
  return <Container maxWidth="lg">
    ...
  </Container>;
};


// https://morioh.com/p/7c097570ecd9
const HighlightList = () => {
  const items = [{}, {}, {}, {}];
  return <React.Fragment>
    {items.map((x, i) => <HighlightItem key={i} item={x} />)}
  </React.Fragment>;
}
const HighlightItem = (props: { item: {}; key: number; }) => <Paper>{props.key}</Paper>


// Red/black separator + header text, spacing
const Offerings = () => <Container maxWidth="lg">
  <Paper elevation={3}>
    Matýsek
    <h2>Přípravka tanečního sportu</h2>
    <p>
      První kroky do světa tanečního sportu pro děti od 5 do 10 let. Všeobecná
      taneční průprava a základy tanečních kroků pro budoucí hvězdy
    </p>
  </Paper>

  <Paper elevation={3}>
    Malí
    <h2>Základy tanečního sportu</h2>
    <p>
      Tréninkové programy pro začínající a mírně pokročilé tanečníky ve věkových
      skupinách juniorů (12-15 let), mládež a dospělí (16+ let).
    </p>
  </Paper>

  <Paper elevation={3}>
    Honová
    <h2>Výkonnostní sport</h2>
    <p>
      Tréninkové programy pro soutěžní tanečníky ve všech věkových skupinách a
      výkonnostních třídách uzpůsobené podle potřeb v jednotlivých výkonnostních
      stupních.
    </p>
  </Paper>

  <Paper elevation={3}>
    Šír
    <h2>Sportovní centrum mládeže</h2>
    <p>
      Tréninkový program pro vrcholové sportovce, reprezentanty ČR se špičkovými
      českými trenéry, speciální kondiční přípravou a moderními metodami
      sportovního tréninku. Jsme jediným klubem v Olomouckém kraji se statutem
      Sprtovního centra mládeže dle MŠMT.
    </p>
  </Paper>
</Container>


const HeroArticles = () => <Container maxWidth="lg">
  <Carousel>{items.map((item, i) => <HeroItem key={i} item={item} />)}</Carousel>;
</Container>;

// https://www.npmjs.com/package/react-material-ui-carousel
const items = [
  {
    name: "Random Name #1",
    description: "Probably the most random thing you have ever seen!"
  },
  {
    name: "Random Name #2",
    description: "Hello World!"
  }
];

const HeroItem = (props: { item: { name: string; description: string; } }) => <div>
  <h2>{props.item.name}</h2>
  <p>{props.item.description}</p>
  <Button className="CheckButton">Check it out!</Button>
</div>;



const useStyles2 = makeStyles((theme) => ({
  cta: {
    backgroundColor: theme.palette.primary.main,
  },
}));

const CallToAction = () => {
  const classes = useStyles2();
  return <div className={classes.cta}>
    <div>Přidej se k nám</div>
    <div>a objev lásku k tanci</div>
    <Button>Chci tančit</Button>
  </div>
};
