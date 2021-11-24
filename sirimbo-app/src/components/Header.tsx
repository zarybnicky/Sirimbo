import * as React from 'react';
import { Link } from 'react-router-dom';
import { AppBar, Container, Toolbar, Typography } from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';
import { gql } from 'graphql-tag';

import { Button } from './Button';

interface HeaderProps {
  user?: {};
  onLogin: () => void;
  onLogout: () => void;
  onCreateAccount: () => void;
}

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

export const Header = ({ user, onLogin, onLogout, onCreateAccount }: HeaderProps) => {
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
          {user
            ? <Button size="small" onClick={onLogout} label="Log out" />
            : <React.Fragment>
              <Button size="small" onClick={onLogin} label="Log in" />
              <Button primary size="small" onClick={onCreateAccount} label="Sign up" />
            </React.Fragment>
          }
          Icon
          <div>Přihlásit</div>
        </div>
      </Container>
    </Toolbar>
  </AppBar>;
};
