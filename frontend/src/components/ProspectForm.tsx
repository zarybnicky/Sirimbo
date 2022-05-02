import * as React from 'react';
import type { CellPlugin } from '@react-page/editor';
import { Card, CardContent, CardActions, Container, Grid, Button, Typography } from '@material-ui/core';
import { TextField } from 'mui-rff';
import { Alert } from '@material-ui/lab';
import { Form } from 'react-final-form'
import { useTypedMutation } from '../zeus/apollo';
import { $, CrmCohort, Selector } from '../zeus';
import { useSnackbar } from 'notistack';

export const SubmitProspectForm = Selector('Mutation')({
  prospectFormDancer: [{
    input: {
      cohort: CrmCohort.DANCER,
      prospectData: $`prospectData`,
      origin: $`origin`,
      note: '',
    }
  }, {
    clientMutationId: true,
  }],
});

export const ProspectForm = ({ }) => {
  const { enqueueSnackbar } = useSnackbar();
  const [submit] = useTypedMutation(SubmitProspectForm);
  const onSubmit = async (prospectData: object) => {
    await submit({ variables: { prospectData, origin: window.location.toString() } });
    enqueueSnackbar('Brzy se vám ozveme!', { variant: 'success' });
  };

  return (
    <Container maxWidth="lg">
      <Form onSubmit={onSubmit} render={form => <>
        {form.submitError && <Alert severity="error">{form.submitError}</Alert>}
        <Card component="form" elevation={3} onSubmit={form.handleSubmit}>
          <CardContent>
            <Typography variant="h4" component="div">Přijď tančit!</Typography>
            <Grid container spacing={3}>
              <Grid item md><TextField label="Jméno" name="name" autoComplete="given-name" required /></Grid>
              <Grid item md><TextField label="Příjmení" name="surname" autoComplete="family-name" required /></Grid>
              <Grid item md><TextField label="Rok narození" name="yearofbirth" autoComplete="bday-year" required /></Grid>
            </Grid>
            <Grid container spacing={3} style={{ marginTop: '1rem' }}>
              <Grid item md><TextField label="Telefon" name="phone" type="tel" autoComplete="tel" required /></Grid>
              <Grid item md><TextField label="E-mail" name="email" type="email" autoComplete="email" required /></Grid>
            </Grid>
          </CardContent>
          <CardActions style={{ flexDirection: 'column' }}>
            <Button
              fullWidth variant="contained" type="submit" color="primary"
              disabled={form.pristine || form.submitting || form.hasValidationErrors}
            >Chci tančit!</Button>
          </CardActions>
        </Card>
      </>} />
    </Container >
  );
};

export const ProspectFormPlugin: CellPlugin<{}> = {
  Renderer: () => <ProspectForm />,

  id: 'app-prospect-form',
  title: 'Prospect Form',
  description: undefined,
  version: 1,
};
