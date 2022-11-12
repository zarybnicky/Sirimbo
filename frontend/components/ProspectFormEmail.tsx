import * as React from 'react';
import type { CellPlugin } from '@react-page/editor';
import { Card, CardContent, CardActions, Grid, Typography } from '@mui/material';
import { useSnackbar } from 'notistack';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { CrmCohort, useSubmitProspectFormMutation } from 'lib/graphql';
import { ErrorBox } from './ErrorBox';
import { useAsyncCallback } from 'react-async-hook';
import { SubmitButton } from './SubmitButton';

type ProspectFormEmailProps = {
  title?: string;
};

export const ProspectFormEmail = ({ title }: ProspectFormEmailProps) => {
  const { enqueueSnackbar } = useSnackbar();
  const { mutateAsync: submit } = useSubmitProspectFormMutation();
  const { control, handleSubmit, formState } = useForm();

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
        <Grid container spacing={1.5}>
          <Grid item xs={12} sm={6}>
            <TextFieldElement control={control} name="name" label="Jméno" autoComplete="given-name" required />
          </Grid>
          <Grid item xs={12} sm={6}>
            <TextFieldElement control={control} name="surname" label="Příjmení" autoComplete="family-name" required />
          </Grid>
        </Grid>
        <Grid container spacing={1.5}>
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
        <SubmitButton className="w-full" loading={onSubmit.loading} disabled={!formState.isDirty || !formState.isValid}>
          Ozvěte se mi
        </SubmitButton>
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
