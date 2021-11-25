import * as React from 'react';
import { Container, Typography } from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';
import clsx from 'clsx';

const useStyles = makeStyles((theme) => ({
  section: {
    color: theme.palette.secondary.contrastText,
    backgroundColor: theme.palette.secondary.main
  },
  h2: {
    fontSize: '2rem',
    fontWeight: 900,
  },
  h3: {
    color: theme.palette.primary.main,
    fontWeight: 700,
    fontSize: '1.2rem',
    marginTop: '1rem',
  },
  h4: {
    fontSize: '1.2rem',
    fontWeight: 900,
    marginTop: '.5rem',
  },
  gutter: {
    marginTop: '1rem',
  }
}));

export const Footer = () => {
  const classes = useStyles();

  return <div className={classes.section}>
    <Container maxWidth="lg">
      <Typography variant="h2" className={classes.h2}>Kontakt:</Typography>

      <Typography variant="h3" className={clsx(classes.h3, classes.gutter)}>Taneční klub</Typography>
      <Typography variant="h4" className={classes.h4}>Taneční klub Olymp Olomouc</Typography>
      <Typography>
        Jiráskova 25, 779 00 Olomouc<br />
        IČO: 68347286<br />
        tkolymp@tkolymp.cz
      </Typography>

      <br />
      <br />

      <Typography variant="h3" className={classes.h3}>Taneční sály</Typography>
      <Typography variant="h4" className={classes.h4}>Taneční centrum při FZŠ Holečkova</Typography>
      <Typography>
        Holečkova 10, 779 00 Olomouc<br />
        (vchod brankou u zastávky Povel, škola)
      </Typography>

      <br />
      <Typography variant="h4" className={classes.h4}>Tělocvična Slovanského gymnázia</Typography>
      <Typography>
        Jiřího z Poděbrad 13, 779 00 Olomouc<br />
        (vchod bránou z ulice U reálky)
      </Typography>
    </Container>
  </div>;
};
