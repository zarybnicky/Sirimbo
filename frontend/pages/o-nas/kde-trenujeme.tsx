import * as React from 'react';
import { Container } from '@mui/material';
import { LocationCard } from 'components/cards/LocationCard';
import { useLocations } from 'lib/data';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';

export const LocationsPage = ({ }) => {
  const items = useLocations();
  return <React.Fragment>
    <Heading color={{ r: 20, g: 200, b: 20, a: .5 }} text="Kde trénujeme" image="" />
    <Container maxWidth="md" style={{ paddingBottom: '2rem', paddingTop: '2rem' }}>
      {items.map((x, i) => <LocationCard item={x} key={i} />)}
    </Container>
    <CallToAction />
  </React.Fragment>;
};
