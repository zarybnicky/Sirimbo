import * as React from 'react';
import { gql, useQuery } from '@apollo/client';
import { makeStyles, Container, Typography, Grid } from '@material-ui/core';
import { useParams } from 'react-router';

const useStyles = makeStyles(() => ({
  section: {
    margin: '2rem 0 1.25rem'
  },
}));

export const GET_PHOTO = gql(`
query GetPhoto($id: BigInt!) {
  galerieFotoByGfId(gfId: $id) {
    gfId
    gfIdRodic
    gfKdo
    gfName
    gfPath
    gfTimestamp
    galerieDirByGfIdRodic {
      gdId
    }
  }
}`);

export const GalleryPhotoPage = () => {
  const classes = useStyles();
  const { id } = useParams<{ id: string; }>()
  const { data } = useQuery(GET_PHOTO, { variables: { id: parseInt(id, 10) } });
  return <Container maxWidth="lg" style={{ paddingBottom: '2rem', paddingTop: '2rem' }}>
    <Typography className={classes.section} variant="h4" component="h2">Kluboví trenéři</Typography>
    <Grid container spacing={3}>
      <Grid item sm={12} md={6}>
        <img src={data?.galerieFotoByGfId?.gfPath} alt={data?.galerieFotoByGfId?.gfName} />
      </Grid>
    </Grid>;
  </Container>;
};
