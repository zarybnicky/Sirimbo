import * as React from 'react';
import { Card } from 'components/Card';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useSubmitProspectFormMutation } from 'lib/graphql/Crm';
import { ErrorBox } from './ErrorBox';
import { useAsyncCallback } from 'react-async-hook';
import { SubmitButton } from './SubmitButton';
import { toast } from 'react-toastify';
import { CrmCohort } from 'lib/graphql';

type ProspectFormEmailProps = {
  title?: string;
};

export const ProspectFormEmail = ({
  title = 'Nemůžete přijít? Zanechte nám kontakt:',
}: ProspectFormEmailProps) => {
  const { mutateAsync: submit } = useSubmitProspectFormMutation();
  const { control, handleSubmit } = useForm();

  const onSubmit = useAsyncCallback(async ({ op, ...prospectData }: any) => {
    if (typeof fbq !== 'undefined') {
      fbq('track', 'Lead');
    }
    await submit({
      cohort: CrmCohort.ContactMeLater,
      prospectData,
      origin: window.location.toString(),
    });
    toast.success('Brzy se vám ozveme!');
  });

  return (
    <Card>
      <form className="grid grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
        <h4 className="text-xl font-bold mb-2 col-full">{title}</h4>

        <TextFieldElement
          control={control}
          name="name"
          label="Jméno"
          autoComplete="given-name"
          required
        />
        <TextFieldElement
          control={control}
          name="surname"
          label="Příjmení"
          autoComplete="family-name"
          required
        />
        <TextFieldElement
          className="col-full"
          control={control}
          name="email"
          type="email"
          autoComplete="email"
          required
        />
        <CheckboxElement
          className="col-full mt-4 mb-2"
          control={control}
          name="op"
          value="agreed"
          required
          label={
            <>
              Souhlasím se{' '}
              <a
                className="text-red-500"
                rel="noreferrer"
                target="_blank"
                href="/ochrana-osobnich-udaju"
              >
                zpracováním osobních údajů
              </a>
            </>
          }
        />
        <ErrorBox error={onSubmit.error} />
        <SubmitButton className="col-full w-full" loading={onSubmit.loading}>
          Ozvěte se mi
        </SubmitButton>
      </form>
    </Card>
  );
};
