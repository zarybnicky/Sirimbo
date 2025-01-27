import { type PersonFragment, UpdatePersonDocument } from '@/graphql/Person';
import { useZodForm } from '@/lib/use-schema-form';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { countries } from '@/lib/countries';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { type TypeOf, z } from 'zod';

const Form = z.object({
  prefixTitle: z.string().default(''),
  firstName: z.string(),
  lastName: z.string(),
  suffixTitle: z.string().default(''),
  gender: z.enum(['MAN', 'WOMAN']),
  birthDate: z.string().nullish(),
  email: z.string().email().nullish(),
  phone: z.string().min(9).max(14).nullish(),
  cstsId: z
    .string()
    .regex(/[0-9]{8}/, 'Neplatné IDT')
    .nullish(),
  wdsfId: z
    .string()
    .regex(/[0-9]{8}/, 'Neplatný MIN')
    .nullish(),
  taxIdentificationNumber: z
    .string()
    .regex(/[0-9]{9,10}/, 'Neplatné rodné číslo')
    .nullish(),
  nationality: z.string(),
  bio: z.string().default(''),
  address: z.object({
    city: z.string().nullish(),
    conscriptionNumber: z.string().nullish(),
    district: z.string().nullish(),
    orientationNumber: z.string().nullish(),
    postalCode: z.string().nullish(),
    region: z.string().nullish(),
    street: z.string().nullish(),
  }),
});

export function EditPersonForm({ data }: { data: PersonFragment }) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useZodForm(Form, {
    defaultValues: data as unknown as any,
  });
  const update = useMutation(UpdatePersonDocument)[1];

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await update({ input: { id: data.id, patch: values } });
    onSuccess();
  });

  return (
    <form className="grid lg:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <TextFieldElement control={control} name="prefixTitle" label="Titul před jménem" />
      <TextFieldElement control={control} name="suffixTitle" label="Titul za jménem" />
      <TextFieldElement control={control} name="firstName" label="Jméno" required autoFocus />
      <TextFieldElement control={control} name="lastName" label="Příjmení" required />

      <TextFieldElement control={control} name="email" type="email" label="E-mail" />
      <TextFieldElement control={control} name="phone" type="tel" label="Telefon" />

      <TextFieldElement type="date" control={control} label="Datum narození" name="birthDate" />
      <TextFieldElement control={control} name="taxIdentificationNumber" label="Rodné číslo" placeholder="1111119999" />

      <TextFieldElement control={control} name="cstsId" label="ČSTS IDT" placeholder="10000000" />
      <TextFieldElement control={control} name="wdsfId" label="WDSF MIN" placeholder="10000000" />

      <div className="col-full">
        <RadioButtonGroupElement
          control={control}
          name="gender"
          options={[
            { id: 'MAN', label: 'Muž' },
            { id: 'WOMAN', label: 'Žena' },
          ]}
        />
      </div>

      <TextFieldElement control={control} name="address.street" label="Ulice" />
      <div className="grid gap-2 md:grid-cols-2">
        <TextFieldElement control={control} name="address.conscriptionNumber" label="Č. popisné" />
        <TextFieldElement control={control} name="address.orientationNumber" label="Č. orientační" />
      </div>

      <TextFieldElement control={control} name="address.district" label="Část města" />
      <TextFieldElement control={control} name="address.city" label="Město" />

      <TextFieldElement control={control} name="address.postalCode" label="PSČ" />
      <TextFieldElement control={control} name="address.region" label="Kraj" />

      <div className="col-full">
        <ComboboxElement
          control={control}
          label="Národnost"
          name="nationality"
          placeholder="vyberte národnost"
          options={countries.map((x) => ({ id: x.code.toString(), label: x.label }))}
        />
      </div>

      <div className="col-full">
        <SubmitButton loading={onSubmit.loading} />
      </div>
    </form>
  );
}
