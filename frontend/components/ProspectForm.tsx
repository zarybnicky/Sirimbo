import * as React from 'react';
import type { CellPlugin } from '@react-page/editor';
import { Alert, Card, CardContent, CardActions, Grid, Button, Typography } from '@mui/material';
import { $, CrmCohort, Selector } from 'lib/zeus';
import { useSnackbar } from 'notistack';
import { useForm } from 'react-hook-form';
import { TextFieldElement, CheckboxElement } from 'react-hook-form-mui';
import { useTypedMutation } from 'lib/query';

export const SubmitProspectForm = Selector('Mutation')({
  prospectFormDancer: [{
    input: {
      cohort: CrmCohort.FREE_LESSON,
      prospectData: $('prospectData', 'JSON!'),
      origin: $('origin', 'String!'),
      note: '',
    }
  }, {
    clientMutationId: true,
  }],
});

type ProspectFormProps = {
  title?: string;
};

export const ProspectForm = ({ title }: ProspectFormProps) => {
  const { enqueueSnackbar } = useSnackbar();
  const { mutateAsync: submit } = useTypedMutation(['prospectForm'], SubmitProspectForm);
  const [submitError, setSubmitError] = React.useState<string | null>(null);
  const { control, handleSubmit, formState: { isDirty, isValid, isSubmitting } } = useForm();

  const onSubmit = async ({ op, ...prospectData }: any) => {
    setSubmitError(null);
    try {
      if (typeof fbq !== 'undefined') {
        fbq('track', 'SubmitApplication');
      }
      await submit({ variables: { prospectData, origin: window.location.toString() } });
      enqueueSnackbar('Brzy se vám ozveme!', { variant: 'success' });
    } catch (e) {
      if (e instanceof Error) {
        setSubmitError(e.message);
      } else {
        setSubmitError('Něco se nepovedlo, zkuste to prosím znovu');
      }
    }
  };

  return (
    <Card component="form" elevation={3} onSubmit={handleSubmit(onSubmit)}>
      <CardContent>
        <Typography variant="h4" component="div">{title}</Typography>
        <Grid container spacing={3}>
          <Grid item xs={12} sm={4}>
            <TextFieldElement control={control} name="name" label="Jméno" autoComplete="given-name" required />
          </Grid>
          <Grid item xs={12} sm={4}>
            <TextFieldElement control={control} name="surname" label="Příjmení" autoComplete="family-name" required />
          </Grid>
          <Grid item xs={12} sm={4}>
            <TextFieldElement control={control} name="yearofbirth" label="Rok narození" autoComplete="bday-year" required />
          </Grid>
        </Grid>
        <Grid container spacing={3}>
          <Grid item xs={12} sm={6}>
            <TextFieldElement control={control} name="phone" type="tel" autoComplete="tel" required />
          </Grid>
          <Grid item xs={12} sm={6}>
            <TextFieldElement control={control} name="email" type="email" autoComplete="email" required />
          </Grid>
        </Grid>
        <Grid container style={{ marginTop: '1rem' }}>
          <Grid item xs={12}>
            <CheckboxElement control={control} name="op" value="agreed" required label={
              <>Souhlasím se <a target="_blank" rel="noreferrer" href="/ochrana-osobnich-udaju">zpracováním osobních údajů</a></>
            } />
          </Grid>
        </Grid>
      </CardContent>
      <CardActions style={{ flexDirection: 'column' }}>
        {submitError && <Alert severity="error">{submitError}</Alert>}
        <Button
          fullWidth variant="contained" type="submit" color="primary"
          disabled={isValid && !isSubmitting && isDirty}
        >Chci tančit!</Button>
      </CardActions>
    </Card>
  );
};

export const ProspectFormPlugin: CellPlugin<ProspectFormProps> = {
  Renderer: ({ data }) => <ProspectForm {...data} />,

  id: 'app-prospect-form',
  title: 'Prospect Form',
  description: undefined,
  version: 2,

  createInitialData: () => ({
    title: 'Chci úvodní hodinu ZDARMA!',
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
