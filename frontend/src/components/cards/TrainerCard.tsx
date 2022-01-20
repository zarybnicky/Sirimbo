import * as React from 'react';
import { makeStyles, Typography, Paper } from '@material-ui/core';
import { SlateReadonly } from '../SlateReadonly';
import { Descendant } from 'slate';

const useStyles = makeStyles((theme) => ({
  item: {
    position: 'relative',
    height: '100%',
    '& .image': {
      position: 'absolute',
      top: '1rem',
      right: '1rem',
      width: '100px',
      height: '120px',
      objectFit: 'cover',
    },
    '& .header': {
      backgroundColor: theme.palette.secondary.main,
      color: theme.palette.secondary.contrastText,
      borderLeft: '8px solid',
      borderLeftColor: theme.palette.primary.main,
      padding: '1rem',
    },
    '& h3': {
      fontWeight: 'bold',
    },
    '& [data-slate-editor="true"]': {
      paddingTop: '.5rem',
      paddingRight: '1rem',
    },
    '& [data-slate-editor="true"] li:nth-of-type(1)': {
      paddingRight: '115px',
    },
    '& [data-slate-editor="true"] li:nth-of-type(2)': {
      paddingRight: '115px',
    },
  }
}));

interface Card {
  img: string;
  name: string;
  content: Descendant[];
}

export const TrainerCard = ({ item: x }: { item: Card; }) => {
  const classes = useStyles();
  return <Paper elevation={3} className={classes.item}>
    <div className="header">
      <Typography variant="h6" component="h3">{x.name}</Typography>
    </div>
    <SlateReadonly value={x.content} />
    <img className="image" src={x.img} alt={x.name} />
  </Paper>;
};
