import * as React from 'react';
import { Card, CardContent, CardActions, Grid, Button, Typography } from '@material-ui/core';
import { Checkboxes, TextField } from 'mui-rff';
import { Alert } from '@material-ui/lab';
import { Form } from 'react-final-form'
import { useTypedMutation } from '../zeus/apollo';
import { $, CrmCohort, Selector } from '../zeus';
import { useSnackbar } from 'notistack';

export const SubmitProspectForm = Selector('Mutation')({
  submitForm: [{
    input: {
      data: $`prospectData`,
      type: 'Přijď tančit!',
      url: $`origin`
    },
  }, {
    clientMutationId: true
  }],
});

type ProspectFormProps = {
  title?: string;
};

export const ProspectForm = ({ title }: ProspectFormProps) => {
  const { enqueueSnackbar } = useSnackbar();
  const [submit] = useTypedMutation(SubmitProspectForm);
  const onSubmit = async ({ op, ...prospectData }: any) => {
    await submit({ variables: { prospectData, origin: window.location.toString() } });
    if (typeof fbq !== 'undefined') {
      fbq('track', 'SubmitApplication');
    }
    enqueueSnackbar('Brzy se vám ozveme!', { variant: 'success' });
  };

  return (
    <Form onSubmit={onSubmit} render={form => <>
      {form.submitError && <Alert severity="error">{form.submitError}</Alert>}
      <Card component="form" elevation={3} onSubmit={form.handleSubmit}>
        <CardContent>
          <Typography variant="h4" component="div">{title}</Typography>
          <Grid container spacing={3}>
            <Grid item xs={12} sm={4}>
              <TextField label="Jméno" name="name" autoComplete="given-name" required />
            </Grid>
            <Grid item xs={12} sm={4}>
              <TextField label="Příjmení" name="surname" autoComplete="family-name" required />
            </Grid>
            <Grid item xs={12} sm={4}>
              <TextField label="Rok narození" name="yearofbirth" autoComplete="bday-year" required />
            </Grid>
          </Grid>
          <Grid container spacing={3}>
            <Grid item xs={12} sm={6}>
              <TextField label="Telefon" name="phone" type="tel" autoComplete="tel" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextField label="E-mail" name="email" type="email" autoComplete="email" required />
            </Grid>
          </Grid>
          <Grid container style={{ marginTop: '1rem' }}>
            <Grid item xs={12}>
              <Checkboxes name="op" required={true} data={{
                label: <>Souhlasím se <a target="_blank" href="/ochrana-osobnich-udaju">zpracováním osobních údajů</a></>,
                value: "agreed"
              }} />
            </Grid>
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
  );
};