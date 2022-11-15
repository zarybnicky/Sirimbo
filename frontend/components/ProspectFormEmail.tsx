import * as React from 'react';
import type { CellPlugin } from '@react-page/editor';
import { Card } from 'components/Card';
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
    <Card>
      <form className="grid grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
        <h4 className="col-full">{title}</h4>

        <TextFieldElement control={control} name="name" label="Jméno" autoComplete="given-name" required />
        <TextFieldElement control={control} name="surname" label="Příjmení" autoComplete="family-name" required />
        <TextFieldElement className="col-full" control={control} name="email" type="email" autoComplete="email" required />
        <CheckboxElement className="col-full mt-4 mb-2" control={control} name="op" value="agreed" required label={
          <>Souhlasím se <a className="text-red-500" rel="noreferrer" target="_blank" href="/ochrana-osobnich-udaju">zpracováním osobních údajů</a></>
        } />
        <ErrorBox error={onSubmit.error} />
        <SubmitButton className="col-full w-full" loading={onSubmit.loading} disabled={!formState.isDirty || !formState.isValid}>
          Ozvěte se mi
        </SubmitButton>
      </form>
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
