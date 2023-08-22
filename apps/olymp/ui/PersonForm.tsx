import React from 'react';
import { ComboboxElement } from '@app/ui/Combobox';
import { RadioButtonGroupElement } from '@app/ui/RadioButtomGroupElement';
import { TextFieldElement } from '@app/ui/fields/text';
import { useCountries } from '@app/ui/use-countries';
import { PersonDocument } from '@app/graphql/Person';
import { useQuery } from 'urql';
import { ErrorPage } from './ErrorPage';
import { z } from 'zod';
import { useZodForm } from 'lib/use-schema-form';

const Form = z.object({
  firstName: z.string(),
  lastName: z.string(),
  birthDate: z.string(),
  nationalIdNumber: z
    .string()
    .regex(/[0-9]{9,10}/, 'Neplatné rodné číslo')
    .nullish(),
  gender: z.enum(['MAN', 'WOMAN']),
  nationality: z.string(),
});

export const PersonForm = ({ id = '', onSuccess }: { id?: string; onSuccess?: () => void; }) => {
  const [query] = useQuery({ query: PersonDocument, variables: { id } });
  const data = query.data?.person;

  const countries = useCountries();
  const { reset, control } = useZodForm(Form);
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [reset, data]);

  if (query.data && query.data.person === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="grid lg:grid-cols-2 gap-2">
      <TextFieldElement control={control} name="firstName" label="Jméno" required />
      <TextFieldElement control={control} name="lastName" label="Příjmení" required />

      <TextFieldElement
        type="date"
        control={control}
        label="Datum narození"
        name="birthDate"
        required
      />
      <TextFieldElement
        control={control}
        name="nationalIdNumber"
        label="Rodné číslo"
        required
        placeholder="1111119999"
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
      <div className="col-full">
        <ComboboxElement
          control={control}
          label="Národnost"
          name="nationality"
          placeholder="vyberte národnost"
          options={countries.map((x) => ({ id: x.code.toString(), label: x.label }))}
        />
      </div>
    </form>
  );
};
