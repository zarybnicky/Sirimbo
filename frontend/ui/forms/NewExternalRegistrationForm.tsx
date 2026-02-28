import { type EventFragment, RegisterToEventExternalDocument } from '@/graphql/Event';
import { TextAreaElement } from '@/ui/fields/textarea';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import * as React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { z } from 'zod';
import { ComboboxElement } from '../fields/Combobox';
import { TextFieldElement } from '../fields/text';
import { countries } from '@/lib/countries';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  firstName: z.string(),
  lastName: z.string(),
  prefixTitle: z.string().prefault(''),
  suffixTitle: z.string().prefault(''),
  nationality: z.string(),
  birthDate: z.string().nullish(),
  taxIdentificationNumber: z
    .string()
    .regex(/[0-9]{9,10}/, 'Neplatné rodné číslo')
    .nullish(),
  email: z.email(),
  phone: z.string().min(9).max(14),
  note: z.string().prefault(''),
});

export function NewExternalRegistrationForm({ event }: { event: EventFragment }) {
  const { onSuccess } = useFormResult();
  const create = useMutation(RegisterToEventExternalDocument)[1];

  const { control, watch, handleSubmit } = useForm({
    defaultValues: {
      nationality: '203',
    },
    resolver: zodResolver(Form),
  });

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    const res = await create({
      input: {
        eventExternalRegistration: {
          eventId: event.id,
          ...values,
        },
      },
    });
    if (res.data?.createEventExternalRegistration) {
      toast.success('Přihlášení na akci proběhlo úspěšně.');
      onSuccess();
    }
  });

  return (
    <form onSubmit={handleSubmit(onSubmit.execute)}>
      <fieldset className="grid lg:grid-cols-2 gap-2">
        <FormError error={onSubmit.error} />

        <TextFieldElement
          control={control}
          name="prefixTitle"
          label="Titul před jménem"
        />
        <TextFieldElement control={control} name="suffixTitle" label="Titul za jménem" />
        <TextFieldElement
          control={control}
          name="firstName"
          label="Jméno"
          required
          autoFocus
        />
        <TextFieldElement control={control} name="lastName" label="Příjmení" required />

        <TextFieldElement control={control} name="email" type="email" label="E-mail" />
        <TextFieldElement control={control} name="phone" type="tel" label="Telefon" />

        <ComboboxElement
          control={control}
          label="Národnost"
          name="nationality"
          placeholder="vyberte národnost"
          options={countries.map((x) => ({ id: x.code.toString(), label: x.label }))}
        />
        {watch('nationality') === '203' ? (
          <TextFieldElement
            control={control}
            name="taxIdentificationNumber"
            label="Rodné číslo"
            placeholder="1111119999"
          />
        ) : (
          <TextFieldElement
            control={control}
            name="birthDate"
            label="Datum narození"
            type="date"
          />
        )}

        <div className="col-full">
          <TextAreaElement control={control} name="note" label="Poznámky" />
        </div>
      </fieldset>

      <div className="col-full pt-2">
        <SubmitButton loading={onSubmit.loading} />
      </div>
    </form>
  );
}
