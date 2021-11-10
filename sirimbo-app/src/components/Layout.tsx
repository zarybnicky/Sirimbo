import * as React from 'react';
import { Link } from 'react-router-dom';
import { AppBar, Container, Paper, Button, Toolbar, Typography } from '@material-ui/core';
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

export const AppFooter = () => <div />;

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
export const HomePage = () => <div>
  <Carousel>{items.map((item, i) => <Item key={i} item={item} />)}</Carousel>
</div>;

const Item = (props: { item: { name: string; description: string; } }) => <Paper>
  <h2>{props.item.name}</h2>
  <p>{props.item.description}</p>
  <Button className="CheckButton">Check it out!</Button>
</Paper>;
