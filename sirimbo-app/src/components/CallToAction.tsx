import * as React from 'react';
import { Button, makeStyles } from '@material-ui/core';

const useStyles = makeStyles((theme) => ({
  cta: {
    backgroundColor: theme.palette.primary.main,
  },
}));

export const CallToAction = () => {
  const classes = useStyles();
  return <div className={classes.cta}>
    <div>Přidej se k nám</div>
    <div>a objev lásku k tanci</div>
    <Button>Chci tančit</Button>
  </div>
};
