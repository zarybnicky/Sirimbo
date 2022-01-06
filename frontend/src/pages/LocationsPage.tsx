import * as React from 'react';
import { Container } from '@material-ui/core';
import { LocationCard } from '../components/LocationCard';
import { useLocations } from '../data';
import { CallToAction } from '../components/CallToAction';

export const LocationsPage = ({ }) => {
  const items = useLocations();
  return <React.Fragment>
    <Container maxWidth="md" style={{ paddingBottom: '2rem', paddingTop: '2rem' }}>
      {items.map((x, i) => <LocationCard item={x} key={i} />)}
    </Container>
    <CallToAction />
  </React.Fragment>;
};
