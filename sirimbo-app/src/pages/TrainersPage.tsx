import * as React from 'react';
import { makeStyles, Container, Typography, Paper, Grid } from '@material-ui/core';
import { SlateReadonly } from '../components/SlateReadonly';
import { Descendant } from 'slate';

const useStyles = makeStyles((theme) => ({
  section: {
    margin: '2rem 0 1.25rem'
  },
  item: {
    position: 'relative',
    height: '100%',
    '& .image': {
      position: 'absolute',
      top: '1rem',
      right: '1rem',
      width: '100px',
      height: '100px',
    },
    '& .image img': {
      width: '100%',
      height: '100%',
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

export const TrainersPage = ({ }) => {
  const classes = useStyles();
  const renderList = (items: {
    img: string; name: string; content: Descendant[];
  }[]) => <Grid container spacing={3}>
      {items.map((x, i) => (
        <Grid item sm={12} md={6} key={i}>
          <Paper elevation={3} className={classes.item}>
            <div className="header">
              <Typography variant="h6" component="h3">{x.name}</Typography>
            </div>
            <SlateReadonly value={x.content} />
            <img className="image" src={x.img} alt={x.name} />
          </Paper>
        </Grid>
      ))}
    </Grid>;
  return <Container maxWidth="lg">
    <Typography className={classes.section} variant="h4" component="h2">Kluboví trenéři</Typography>
    {renderList(internal)}
    <Typography className={classes.section} variant="h4" component="h2">Externí trenéři</Typography>
    {renderList(external)}
  </Container>;
};

const internal = [
  {
    img: 'https://picsum.photos/360/240?random=3',
    name: 'Mgr. Miroslav Hýža',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Předseda, šéftrenér TK Olymp' },
        { text: 'Trenér mistrů ČR' },
        { text: 'Vicemistr ČR ve standardních tancích v hlavní kategorii 2018' },
        { text: '3x mistr ČR v kategorii juniorů' },
        { text: 'Finalista Akademického mistrovství Evropy 2016' },
        { text: 'Držitel ocenění TOP 30 sportovních trenérů mládeže 2017' },
      ]
    }],
  },
  {
    img: 'https://picsum.photos/360/240?random=4',
    name: 'Ing. Filip Karásek',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Trenér a porotce II. třídy' },
        { text: '2x mistr ČR v latinsko-amerických tancích v kategorii profesionálů' },
        { text: '3x mistr ČR v latinsko-amerických tancích v hlavní kategorii' },
        { text: 'Semifinalista mistrovství světa a Evropy v kategorii profesionálů' },
        { text: 'Semifinalista mistrovství Evropy v hlavní kategorii' },
        { text: 'Čtvrtfinalista mistrovství světa v hlavní kategorii' },
        { text: 'Finalista GOC ve Stuttgartu kategorii profesionálů' },
        { text: 'Semifinalista Světových her v Kolumbii' },
      ]
    }],
  },
  {
    img: 'https://picsum.photos/360/240?random=5',
    name: 'Mgr. Marie Hýžová',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Trenér a porotce I. třídy' },
        { text: 'Dlouholetá tanečnice mezinárodní třídy' },
        { text: 'Zkušená trenérka dětských a juniorských přípravek' },
        { text: 'Švadlena tanečního oblečení (řadu let šatí finalisty i medailisty mistrovství ČR)' },
      ]
    }],
  },
  {
    img: 'https://picsum.photos/360/240?random=6',
    name: 'Mgr. Lucie Benýšková',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolventka kvalifikačního studia pro trenéry a porotce II.třídy' },
        { text: 'Semifinalistka mistrovství ČR v hlavní kategorii (v deseti tancích a ve standardních tancích)' },
        { text: '3x finalistka mistrovství ČR juniorů a mládeže (titul druhý vicemistr ČR ve standardních tancích v kategorii mládež)' },
        { text: '2. místo v Taneční lize (žebříček párů mezinárodní třídy)' },
        { text: 'Trenérka dětských a juniorských přípravek (12 let praxe)' },
        { text: 'Trenérka finalistů mistrovství ČR juniorů' },
        { text: 'Absolventka Fakulty tělesné kultury na UP v Olomouci (obor: Tělesná výchova a sport)' },
      ]
    }],
  },
  {
    img: 'https://picsum.photos/360/240?random=7',
    name: 'Mgr. Pavel Grepl',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolvent kvalifikačního studia pro trenéry a porotce II. třídy' },
        { text: 'Tanečník mezinárodní třídy' },
        { text: 'Finalista Akademického mistrovství ČR ve standardních tancích' },
        { text: 'Student Fakulty tělesné kultury na UP v Olomouci (obor: Trenérství a sport)' },
        { text: 'Trenér atletiky III. třídy a plavání III. třídy' },
      ]
    }],
  },
  {
    img: 'https://picsum.photos/360/240?random=8',
    name: 'Bc. Marie Hýžová ml.',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolventka kvalifikačního studia pro trenéry a porotce II. třídy' },
        { text: 'Finalistka mistrovství ČR v deseti tancích' },
        { text: 'Semifinalistka mistrovství ČR ve standardních tancích' },
        { text: 'Čtvrtfinalistka mistrovství ČR v latinskoamerických tancích' },
      ]
    }],
  },
  {
    img: 'https://picsum.photos/360/240?random=9',
    name: 'Roman Pecha',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolvent kvalifikačního studia pro trenéry a porotce II. třídy' },
        { text: 'Tanečník mezinárodní třídy' },
        { text: 'Finalista mistrovství ČR v hlavní kategorii (ve standardních tancích)' },
        { text: '2x mistr ČR v kategorii U21 (v deseti tancích a ve standardních tancích)' },
        { text: 'Čtvrtfinalista mistrovství světa a Evropy v kategorii mládež ve standardních tancích' },
        { text: 'Čtvrtfinalista mistrovství světa v kategorii mládež a U21 v deseti tancích' },
        { text: 'Student MVŠO' },
      ]
    }],
  },
  {
    img: 'https://picsum.photos/360/240?random=10',
    name: 'Hana Anna Šišková',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolventka kvalifikačního studia pro trenéry a porotce III. třídy' },
        { text: 'Tanečnice mezinárodní třídy' },
        { text: '4x finalistka mistrovství ČR v kategorii U21 (titul vicemistr ČR v deseti tancích, titul druhý vicemistr ČR ve standardních tancích a titul druhý vicemistr ČR v latinskoamerických tancích)' },
        { text: 'Studentka Fakulty tělesné kultury na UP v Olomouci (obor: Tělesná výchova a sport)' },
      ]
    }],
  },
  {
    img: 'https://picsum.photos/360/240?random=11',
    name: 'Nela Šírová',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolventka kvalifikačního studia pro trenéry a porotce III. třídy' },
        { text: 'Tanečnice mezinárodní třídy' },
        { text: '3x finalistka mistrovství ČR juniorů a mládeže' },
        { text: 'Studentka Lékařské fakulty na UP v Olomouci' },
      ]
    }],
  },
  {
    img: 'https://picsum.photos/360/240?random=11',
    name: 'Matěj Očenášek',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolvent kvalifikačního studia pro trenéry a porotce III. třídy' },
        { text: '2x finalista mistrovství ČR mládeže' },
        { text: '3x finalista mistrovství ČR v kategorii U21 (vicemistr ČR v deseti tancích)' },
        { text: 'Student VUT FEKT' },
      ]
    }],
  },
];

const external = [
  {
    img: 'https://picsum.photos/360/240?random=12',
    name: 'Martin Odstrčil',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Prezident DSP Kometa Brno' },
        { text: 'Trenér finalistů mistrovství světa a medailistů mistrovství Evropy' },
        { text: 'Trenér mistrů České republiky všech věkových kategorií' },
        { text: '6x mistr ČR v deseti tancích (1995-2000)' },
        { text: 'Mistr ČR ve standardních tancích (2000)' },
        { text: 'Trenér a porotce I. třídy' },
      ]
    }],
  },
  {
    img: 'https://picsum.photos/360/240?random=13',
    name: 'Pavla Landsfeldová',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Trenérka mistrů České republiky ve standardních tancích' },
        { text: 'Finalistka mistrovství ČSSR ve standardních tancích' },
        { text: 'Mistryně ČR ve standardních tancích v kategorii senior (1996)' },
        { text: 'Trenérka a porotkyně I. třídy' },
      ]
    }],
  },
  {
    img: 'https://picsum.photos/360/240?random=14',
    name: 'Ing. Jaroslav Kučera',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Předseda trenérské rady projektu Sportovně talentované mládeže ČSTS' },
        { text: 'Trenér finalistů mistrovství světa a medailistů mistrovství Evropy' },
        { text: 'Trenér mistrů České republiky všech věkových kategorií' },
        { text: 'Vicemistr ČR v latinskoamerických tancích (1992, 1993, 1995)' },
        { text: 'Finalista mistrovství ČR v deseti tancích (1993)' },
        { text: 'Trenér a porotce i. třídy' },
      ]
    }],
  },
];
