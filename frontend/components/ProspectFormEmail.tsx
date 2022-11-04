import * as React from 'react';
import type { CellPlugin } from '@react-page/editor';
import { Card, CardContent, CardActions, Grid, Button, Typography } from '@mui/material';
import { useSnackbar } from 'notistack';
import { useForm } from 'react-hook-form';
import { TextFieldElement, CheckboxElement } from 'react-hook-form-mui';
import { CrmCohort, useSubmitProspectFormMutation } from 'lib/graphql';
import { ErrorBox } from './ErrorBox';
import { useAsyncCallback } from 'react-async-hook';

type ProspectFormEmailProps = {
  title?: string;
};

export const ProspectFormEmail = ({ title }: ProspectFormEmailProps) => {
  const { enqueueSnackbar } = useSnackbar();
  const { mutateAsync: submit } = useSubmitProspectFormMutation();
  const { control, handleSubmit, formState: { isDirty, isValid, isSubmitting } } = useForm();

  const onSubmit = useAsyncCallback(async ({ op, ...prospectData }: any) => {
    if (typeof fbq !== 'undefined') {
      fbq('track', 'Lead');
    }
    await submit({ cohort: CrmCohort.ContactMeLater, prospectData, origin: window.location.toString() });
    enqueueSnackbar('Brzy se vám ozveme!', { variant: 'success' });
  });

  return (
    <Card component="form" elevation={3} onSubmit={handleSubmit(onSubmit.execute)}>
      <CardContent>
        <Typography variant="h4" component="div">{title}</Typography>
        <Grid container spacing={3}>
          <Grid item xs={12} sm={6}>
            <TextFieldElement control={control} name="name" label="Jméno" autoComplete="given-name" required />
          </Grid>
          <Grid item xs={12} sm={6}>
            <TextFieldElement control={control} name="surname" label="Příjmení" autoComplete="family-name" required />
          </Grid>
        </Grid>
        <Grid container spacing={3}>
          <Grid item xs={12}>
            <TextFieldElement control={control} name="email" type="email" autoComplete="email" required />
          </Grid>
        </Grid>
        <Grid container style={{ marginTop: '1rem' }}>
          <Grid item xs={12}>
            <CheckboxElement control={control} name="op" value="agreed" required label={
              <>Souhlasím se <a rel="noreferrer" target="_blank" href="/ochrana-osobnich-udaju">zpracováním osobních údajů</a></>
            } />
          </Grid>
        </Grid>
        <ErrorBox grid error={onSubmit.error} />
      </CardContent>
      <CardActions style={{ flexDirection: 'column' }}>
        <Button
          fullWidth variant="contained" type="submit" color="primary"
          disabled={isValid && !isSubmitting && isDirty}
        >Ozvěte se mi</Button>
      </CardActions>
    </Card>
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
