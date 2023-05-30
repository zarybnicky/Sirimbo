import * as React from 'react';
import { Card } from 'components/Card';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { toast } from 'react-toastify';
import { SubmitFormDocument } from 'lib/graphql/Crm';
import { useMutation } from 'urql';

type ProspectFormProps = {
  title: string;
};

export const ProspectForm = ({ title }: ProspectFormProps) => {
  const submit = useMutation(SubmitFormDocument)[1];
  const { control, handleSubmit } = useForm();

  const onSubmit = useAsyncCallback(async ({ op, ...data }: any) => {
    if (typeof fbq !== 'undefined') {
      fbq('track', 'SubmitApplication');
    }
    const url = window.location.toString();
    await submit({ type: 'Přijď tančit!', data, url });
    toast.success('Brzy se vám ozveme!');
  });

  return (
    <Card>
      <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
        <h4 className="text-xl font-bold mb-2 col-full">{title}</h4>
        <div className="grid md:grid-cols-3 gap-2">
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
            control={control}
            name="yearofbirth"
            label="Rok narození"
            autoComplete="bday-year"
            required
          />
        </div>

        <div className="grid sm:grid-cols-2 gap-2">
          <TextFieldElement
            control={control}
            name="phone"
            label="Telefon"
            type="tel"
            autoComplete="tel"
            required
          />
          <TextFieldElement
            control={control}
            name="email"
            label="E-mail"
            type="email"
            autoComplete="email"
            required
          />
        </div>

        <CheckboxElement
          className="my-2"
          control={control}
          name="op"
          value="agreed"
          required
          label={
            <>
              Souhlasím se{' '}
              <a
                className="text-red-500"
                target="_blank"
                rel="noreferrer"
                href="/ochrana-osobnich-udaju"
              >
                zpracováním osobních údajů
              </a>
            </>
          }
        />
        <ErrorBox error={onSubmit.error} />
        <SubmitButton className="w-full" loading={onSubmit.loading}>
          Chci tančit!
        </SubmitButton>
      </form>
    </Card>
  );
};
