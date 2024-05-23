import { PersonFragment, UpdatePersonDocument } from '@/graphql/Person';
import { useZodForm } from '@/lib/use-schema-form';
import { RadioButtonGroupElement } from '@/ui/RadioButtomGroupElement';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useCountries } from '@/ui/use-countries';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { TypeOf, z } from 'zod';

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
});

export function EditPersonForm({ data }: { data: PersonFragment }) {
  const countries = useCountries();
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useZodForm(Form);
  const update = useMutation(UpdatePersonDocument)[1];

  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [reset, data]);

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
