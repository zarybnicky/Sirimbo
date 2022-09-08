import * as React from 'react';
import { Link } from 'react-router-dom';
import { AppBar, Box, Container, Toolbar, makeStyles } from '@material-ui/core';
import { SocialButtons } from './SocialButtons';
import { AuthButton } from './AuthButton';
import { DesktopMenu } from './DesktopMenu';

import OlympLogoVertical from '../../static/images/olymp-logo-vertical.svg';

const useStyles = makeStyles((theme) => ({
  navbar: {
    justifyContent: 'space-between',
    alignItems: 'center',
    display: "flex",
  },
  logoBox: {
    ...theme.mixins.toolbar,
    position: 'relative',
    overflow: 'visible',
    minWidth: '104px',
  },
  logo: {
    zIndex: 100,
    position: 'absolute',
    top: 0,
    left: 0,
    right: 0,
    width: '104px',
    height: '130px',
    backgroundColor: theme.palette.primary.main,
    color: theme.palette.primary.contrastText,
  },
  logoInner: {
    display: 'block',
    padding: 0,
    margin: 0,
    height: '100%',
    width: '100%',
    position: 'relative',
  },
  svg: {
    filter: 'drop-shadow(0px 6px 6px rgba(0, 0, 0, 0.2))',
    position: 'absolute',
    left: 0,
    bottom: 0,
    width: '104px',
    height: '104px',
    '& *': {
      color: 'white',
      fill: 'white !important',
    },
  },
}));

export const DesktopHeader = ({ }) => {
  const classes = useStyles();
  return <AppBar position="static" color="secondary">
    <Toolbar>
      <Container maxWidth="lg" className={classes.navbar}>
        <div className={classes.logoBox}>
          <Box boxShadow={10} className={classes.logo}>
            <Link to="/" className={classes.logoInner}>
              <OlympLogoVertical className={classes.svg} />
            </Link>
          </Box>
        </div>
        <DesktopMenu />
        <SocialButtons variant="medium" />
        <AuthButton />
      </Container>
    </Toolbar>
  </AppBar >;
};
