import * as React from 'react';
import type { CellPlugin } from '@react-page/editor';
import { Card, CardContent, CardActions, Grid, Button, Typography } from '@mui/material';
import { useSnackbar } from 'notistack';
import { useForm } from 'react-hook-form';
import { TextFieldElement, CheckboxElement } from 'react-hook-form-mui';
import { CrmCohort, useSubmitProspectFormMutation } from 'lib/graphql';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';

type ProspectFormProps = {
  title?: string;
};

export const ProspectForm = ({ title }: ProspectFormProps) => {
  const { enqueueSnackbar } = useSnackbar();
  const { mutateAsync: submit } = useSubmitProspectFormMutation();
  const { control, handleSubmit, formState: { isDirty, isValid, isSubmitting } } = useForm();

  const onSubmit = useAsyncCallback(async ({ op, ...prospectData }: any) => {
    if (typeof fbq !== 'undefined') {
      fbq('track', 'SubmitApplication');
    }
    await submit({ cohort: CrmCohort.FreeLesson, prospectData, origin: window.location.toString() });
    enqueueSnackbar('Brzy se vám ozveme!', { variant: 'success' });
  });

  return (
    <Card component="form" elevation={3} onSubmit={handleSubmit(onSubmit.execute)}>
      <CardContent>
        <Typography variant="h4" component="div">{title}</Typography>
        <Grid container spacing={1.5}>
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
        <Grid container spacing={1.5}>
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
        <ErrorBox grid error={onSubmit.error} />
      </CardContent>
      <CardActions style={{ flexDirection: 'column' }}>
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
