import * as React from 'react';
import { Container } from '@material-ui/core';
import { LocationCard } from '../components/LocationCard';
import { useLocations } from '../data';

export const LocationsPage = ({ }) => {
  const items = useLocations();
  return <Container maxWidth="md" style={{ paddingBottom: '2rem', paddingTop: '2rem' }}>
    {items.map((x, i) => <LocationCard item={x} key={i} />)}
  </Container>;
};
