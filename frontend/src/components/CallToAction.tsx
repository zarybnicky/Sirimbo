import * as React from 'react';
import { Link } from 'react-router-dom';
import { Button, Container, Typography, makeStyles } from '@material-ui/core';

import CtaImage from '../../static/images/call-to-action.png';

const useStyles = makeStyles((theme) => ({
  container: {
    backgroundColor: theme.palette.primary.main,
  },
  background: {
    backgroundImage: `url(${CtaImage})`,
    backgroundPosition: 'bottom right',
    backgroundRepeat: 'no-repeat',
    backgroundSize: 'auto 100%',
    display: 'flex',
    justifyContent: 'left',
  },
  text: {
    padding: '4rem',
    paddingRight: '6rem',
    background: 'linear-gradient(90deg, rgba(216,28,58,0.7) 70%, rgba(0,0,0,0) 100%)',
    [theme.breakpoints.down('sm')]: {
      padding: '2rem',
    }
  },
  firstLine: {
    color: 'white',
    fontWeight: 'bolder',
    fontSize: '2.5rem',
  },
  secondLine: {
    fontWeight: 'bold',
    fontSize: '1.75rem',
    [theme.breakpoints.down('sm')]: {
      fontSize: '1.45rem',
    }
  },
  button: {
    textTransform: 'none',
    marginTop: '1.25rem',
    padding: '.75rem 3rem',
    borderRadius: 0,
    fontSize: '140%',
  }
}));

export const CallToAction = ({ }) => {
  const classes = useStyles();
  return <div className={classes.container}>
    <Container maxWidth="lg" disableGutters className={classes.background}>
      <div className={classes.text}>
        <Typography variant="h4" component="div" className={classes.firstLine}>PŘIDEJ SE K NÁM</Typography>
        <Typography variant="h5" component="div" className={classes.secondLine}>A OBJEV LÁSKU K TANCI</Typography>
        <Button
          component={Link} to="/treninkove-programy"
          color="secondary" variant="contained" size="large"
          className={classes.button}
        >Chci tančit</Button>
      </div>
    </Container>
  </div>;
};