import * as React from 'react';
import type { CellPlugin } from '@react-page/editor';
import { Card, CardContent, CardActions, Grid, Button, Typography } from '@mui/material';
import { Checkboxes, TextField } from 'mui-rff';
import { Alert } from '@mui/lab';
import { Form } from 'react-final-form'
import { useTypedMutation } from 'lib/zeus/apollo';
import { $, CrmCohort, Selector } from 'lib/zeus';
import { useSnackbar } from 'notistack';

export const SubmitProspectFormEmail = Selector('Mutation')({
  prospectFormDancer: [{
    input: {
      cohort: CrmCohort.CONTACT_ME_LATER,
      prospectData: $`prospectData`,
      origin: $`origin`,
      note: '',
    }
  }, {
    clientMutationId: true,
  }],
});

type ProspectFormEmailProps = {
  title?: string;
};

export const ProspectFormEmail = ({ title }: ProspectFormEmailProps) => {
  const { enqueueSnackbar } = useSnackbar();
  const [submit] = useTypedMutation(SubmitProspectFormEmail);
  const onSubmit = async ({ op, ...prospectData }: any) => {
    await submit({ variables: { prospectData, origin: window.location.toString() } });
    if (typeof fbq !== 'undefined') {
      fbq('track', 'Lead');
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
            <Grid item xs={12} sm={6}>
              <TextField label="Jméno" name="name" autoComplete="given-name" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextField label="Příjmení" name="surname" autoComplete="family-name" required />
            </Grid>
          </Grid>
          <Grid container spacing={3}>
            <Grid item xs={12}>
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
          >Ozvěte se mi</Button>
        </CardActions>
      </Card>
    </>} />
  );
};

export const ProspectFormEmailPlugin: CellPlugin<ProspectFormEmailProps> = {
  Renderer: ({ data }) => <ProspectFormEmail {...data} />,

  id: 'app-prospect-form-email',
  title: 'Prospect Form Email',
  description: undefined,
  version: 2,

  createInitialData: () => ({
    title: 'Nemůžete přijít? Zanechte nám kontakt:',
  }),
  controls: {
    type: 'autoform',
    schema: {
      properties: {
        title: {
          type: 'string',
        },
      },
    },
  },
};