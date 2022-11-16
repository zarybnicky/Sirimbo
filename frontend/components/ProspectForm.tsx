import * as React from 'react';
import type { CellPlugin } from '@react-page/editor';
import { Card } from 'components/Card';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { CrmCohort, useSubmitProspectFormMutation } from 'lib/graphql';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { toast } from 'react-toastify';

type ProspectFormProps = {
  title?: string;
};

export const ProspectForm = ({ title }: ProspectFormProps) => {
  const { mutateAsync: submit } = useSubmitProspectFormMutation();
  const { control, handleSubmit, formState } = useForm();

  const onSubmit = useAsyncCallback(async ({ op, ...prospectData }: any) => {
    if (typeof fbq !== 'undefined') {
      fbq('track', 'SubmitApplication');
    }
    await submit({ cohort: CrmCohort.FreeLesson, prospectData, origin: window.location.toString() });
    toast.success('Brzy se vám ozveme!');
  });

  return (
    <Card>
      <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
        <h4 className="col-full">{title}</h4>
        <div className="grid md:grid-cols-3 gap-2">
          <TextFieldElement control={control} name="name" label="Jméno" autoComplete="given-name" required />
          <TextFieldElement control={control} name="surname" label="Příjmení" autoComplete="family-name" required />
          <TextFieldElement control={control} name="yearofbirth" label="Rok narození" autoComplete="bday-year" required />
        </div>

        <div className="grid sm:grid-cols-2 gap-2">
          <TextFieldElement control={control} name="phone" label="Telefon" type="tel" autoComplete="tel" required />
          <TextFieldElement control={control} name="email" label="E-mail" type="email" autoComplete="email" required />
        </div>

        <CheckboxElement className="my-2" control={control} name="op" value="agreed" required label={
          <>Souhlasím se <a className="text-red-500" target="_blank" rel="noreferrer" href="/ochrana-osobnich-udaju">zpracováním osobních údajů</a></>
        } />
        <ErrorBox error={onSubmit.error} />
        <SubmitButton className="w-full" loading={onSubmit.loading} disabled={!formState.isDirty || !formState.isValid}>
          Chci tančit!
        </SubmitButton>
      </form>
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
