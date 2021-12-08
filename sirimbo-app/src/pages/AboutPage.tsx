import * as React from 'react';
import { Container, Grid } from '@material-ui/core';
import { SlateReadonly } from '../components/SlateReadonly';

import MirekDomca from '../../static/images/mirek-domca.png';

export const AboutPage = ({ }) => {
  return <Container maxWidth="md">
    <Grid container direction="row-reverse" justifyContent="center">
      <Grid item sm={12} md={6}>
        <SlateReadonly value={text} />
      </Grid>
      <Grid item sm={12} md={4}>
        <img src={MirekDomca} alt="Miroslav Hýža a Dominika Feglová" />
      </Grid>
    </Grid>
  </Container>;
};

const text = [
  {
    type: 'heading-two',
    children: [
      { text: 'OLYMP', primary: true, bold: true },
      { text: ' V TANEČNÍM SVĚTĚ', bold: true },
    ]
  },
  {
    type: 'paragraph',
    children: [
      { text: 'Jsme tradičním klubem, který se věnuje tanečnímu sportu ', bold: true },
      { text: '- standardním a latinskoamerickým tancům. Více než třicet let se zabýváme se výchovou tanečních sportovců od dětí až po dospělé.' },
      { text: 'Vytvořili jsme provázaný systém tréninkových programů pro začínající, výkonnostní i vrcholové sportovce.' },
      { text: 'Využíváme nejmodernějších tréninkových metod a pravidelně je zdokonalujeme ve spolupráci s odborníky z Fakulty tělesné kultury Univerzity Palackého v Olomouci a mezinárodními trenéry' },
      { text: ' World Dance Sport Organisation.', bold: true },
    ],
  },
  {
    type: 'paragraph',
    children: [
      { text: 'V rámci Českého svazu tanečního sportu jsme jediným klubem z Olomouckého kraje se statutem Sportovní centrum mládeže MŠMT. Pravidelně dodáváme členy národnímu reprezentačnímu týmu a dlouhodobě patříme mezi nejlepší české kluby v práci s mládeží.' },
    ],
  },
  {
    type: 'paragraph',
    children: [
      { text: 'Působíme v Olomouci, Prostějově a Přerově.', bold: true },
    ],
  },
]
