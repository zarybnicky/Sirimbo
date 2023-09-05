import { PersonDocument, PersonFragment, UpdatePersonDocument } from '@app/graphql/Person';
import { ComboboxElement } from '@app/ui/Combobox';
import { RadioButtonGroupElement } from '@app/ui/RadioButtomGroupElement';
import { Dialog, DialogContent, DialogTrigger } from '@app/ui/dialog';
import { TextFieldElement } from '@app/ui/fields/text';
import { buttonCls } from '@app/ui/style';
import { useCountries } from '@app/ui/use-countries';
import { useZodForm } from '@/lib/use-schema-form';
import { Edit } from 'lucide-react';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { TypeOf, z } from 'zod';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from './form';
import { SubmitButton } from './submit';

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
  nationalIdNumber: z
    .string()
    .regex(/[0-9]{9,10}/, 'Neplatné rodné číslo')
    .nullish(),
  nationality: z.string(),
  bio: z.string().default(''),
});

export function EditPersonForm({ data, onSuccess }: { data: PersonFragment; onSuccess?: () => void }) {
  const countries = useCountries();
  const { reset, control, handleSubmit } = useZodForm(Form);
  const update = useMutation(UpdatePersonDocument)[1];

  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [reset, data, open]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await update({ input: { id: data.id, patch: values } });
    onSuccess?.();
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
      <TextFieldElement control={control} name="nationalIdNumber" label="Rodné číslo" placeholder="1111119999" />

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

export function EditPersonButton({ id }: { id: string }) {
  const [open, setOpen] = React.useState(false);
  const [query] = useQuery({ query: PersonDocument, variables: { id }, pause: !id });
  const data = query.data?.person;

  if (!data) return null;

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <button className={buttonCls({ variant: 'outline' })}>
          <Edit />
          Upravit
        </button>
      </DialogTrigger>
      <DialogContent>
        <EditPersonForm data={data} onSuccess={() => setOpen(false)} />
      </DialogContent>
    </Dialog>
  );
};
