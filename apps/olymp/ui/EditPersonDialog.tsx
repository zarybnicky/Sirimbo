import { PersonDocument } from '@app/graphql/Person';
import { ComboboxElement } from '@app/ui/Combobox';
import { RadioButtonGroupElement } from '@app/ui/RadioButtomGroupElement';
import { Dialog, DialogContent, DialogTrigger } from '@app/ui/dialog';
import { TextFieldElement } from '@app/ui/fields/text';
import { buttonCls } from '@app/ui/style';
import { useCountries } from '@app/ui/use-countries';
import { useZodForm } from 'lib/use-schema-form';
import { Edit } from 'lucide-react';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import { ErrorPage } from './ErrorPage';

const Form = z.object({
  firstName: z.string(),
  lastName: z.string(),
  gender: z.enum(['MAN', 'WOMAN']),
  birthDate: z.string().nullish(),
  cstsId: z
    .string()
    .regex(/[0-9]{8}/, 'Neplatné IDT')
    .nullish(),
  wdsfId: z
    .string()
    .regex(/[0-9]{8}/, 'Neplatný MIN')
    .nullish(),
  nationalIdNumber: z
    .string()
    .regex(/[0-9]{9,10}/, 'Neplatné rodné číslo')
    .nullish(),
  nationality: z.string(),
});

export const EditPersonDialog = ({ id = '' }: { id?: string }) => {
  const [open, setOpen] = React.useState(false);
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
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <button className={buttonCls({ variant: 'outline' })}>
          <Edit />
          Upravit
        </button>
      </DialogTrigger>
      <DialogContent>
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

          <TextFieldElement
            control={control}
            name="cstsId"
            label="ČSTS IDT"
            required
            placeholder="10000000"
          />
          <TextFieldElement
            control={control}
            name="wdsfId"
            label="WDSF MIN"
            required
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
      </DialogContent>
    </Dialog>
  );
};
