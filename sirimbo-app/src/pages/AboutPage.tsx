import * as React from 'react';
import { Grid } from '@material-ui/core';
import { SlateReadonly } from '../components/SlateReadonly';

import MirekDomca from '../../static/images/mirek-domca.png';

export const AboutPage = ({ }) => {
  return <Grid container>
    <Grid item sm={12} md={4}>
      <img src={MirekDomca} alt="Miroslav Hýža a Dominika Feglová" />
    </Grid>
    <Grid item sm={12} md={6}>
      <SlateReadonly value={text} />
    </Grid>
  </Grid>;
};

const text = [
  {
    type: 'heading-one',
    children: [
      { text: 'OLYMP', primary: true },
      { text: 'V TANEČNÍM SVĚTĚ', primary: true },
    ]
  },
  {
    type: 'paragraph',
    children: [
      { text: 'Jsme sportovním klubem, který se věnuje tanečnímu sportu', bold: true },
      { text: '- standardním a latinskoamerickým tancům. Více než třicet let se zabýváme se výchovou tanečních sportovců od dětí až po dospělé.' },
    ],
  },
  {
    type: 'paragraph',
    children: [
      { text: 'Stále zdokonalujeme provázaný systém tréninkových programů pro začínající, výkonností i vrcholové sportovce.' },
    ],
  },
  {
    type: 'paragraph',
    children: [
      { text: 'Využíváme nejmodernějších tréninkových metod a pravidelně je posouváme ve spolupráci s odborníky z Falulty tělesné kultury Univerzity Palackého v Olomouci a mezinárodními trenéry' },
      { text: 'World Dance Sport Organisation.', bold: true },
    ],
  },
  {
    type: 'paragraph',
    children: [
      { text: 'V rámci Českého svazu tanečního sportu jsme jedním z nejúspěšnějších klubů posledních let. Naši členové jsou pravidelně členy národního reprezentačního týmu. V Olomouckém kraji jsme jediným klubem tanečního sportu s označením Sportovní centrum mládeže MŠMT.' },
    ],
  },
  {
    type: 'paragraph',
    children: [
      { text: 'Působíme v Olomouci, Prostějově a Přerově.', bold: true },
    ],
  },
]
