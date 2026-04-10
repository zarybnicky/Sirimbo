import { type PersonFragment, UpdatePersonDocument } from '@/graphql/Person';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { TextFieldElement } from '@/ui/fields/text';
import { CstsIdFieldElement } from '@/ui/fields/CstsIdFieldElement';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { countries } from '@/lib/countries';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { RichTextEditor } from '@/ui/fields/richtext';

const Form = z.object({
  prefixTitle: z.string().prefault(''),
  firstName: z.string(),
  lastName: z.string(),
  suffixTitle: z.string().prefault(''),
  gender: z.enum(['MAN', 'WOMAN']),
  birthDate: z.string().nullish(),
  email: z.email().nullish(),
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
  bio: z.string().prefault(''),
  note: z.string().prefault(''),
  address: z.object({
    city: z.string().nullish().prefault(''),
    conscriptionNumber: z.string().nullish().prefault(''),
    district: z.string().nullish().prefault(''),
    orientationNumber: z.string().nullish().prefault(''),
    postalCode: z.string().nullish().prefault(''),
    region: z.string().nullish().prefault(''),
    street: z.string().nullish().prefault(''),
  }),
});

export function EditPersonForm({ data }: { data: PersonFragment }) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useForm({
    defaultValues: data as unknown as any,
    resolver: zodResolver(Form),
  });
  const update = useMutation(UpdatePersonDocument)[1];

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    await update({ input: { id: data.id, patch: values } });
    onSuccess();
  });

  return (
    <form className="grid lg:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <TextFieldElement control={control} name="prefixTitle" label="Titul před jménem" />
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

      <TextFieldElement
        type="date"
        control={control}
        label="Datum narození"
        name="birthDate"
      />
      <TextFieldElement
        control={control}
        name="taxIdentificationNumber"
        label="Rodné číslo"
        placeholder="1111119999"
      />

      <CstsIdFieldElement control={control} name="cstsId" />
      <TextFieldElement
        control={control}
        name="wdsfId"
        label="WDSF MIN"
        placeholder="10000000"
      />

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
        <TextFieldElement
          control={control}
          name="address.conscriptionNumber"
          label="Č. popisné"
        />
        <TextFieldElement
          control={control}
          name="address.orientationNumber"
          label="Č. orientační"
        />
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
        <RichTextEditor
          control={control}
          initialState={data?.note}
          name="note"
          label="Poznámky"
        />
      </div>

      <div className="col-full">
        <SubmitButton loading={onSubmit.loading} />
      </div>
    </form>
  );
}
