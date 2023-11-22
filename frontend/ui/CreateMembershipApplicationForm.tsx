import { CreateMembershipApplicationDocument, DeleteMembershipApplicationDocument, MembershipApplicationFragment, UpdateMembershipApplicationDocument } from '@app/graphql/CurrentUser';
import { ComboboxElement } from '@app/ui/Combobox';
import { RadioButtonGroupElement } from '@app/ui/RadioButtomGroupElement';
import { Dialog, DialogContent, DialogTrigger } from '@app/ui/dialog';
import { TextFieldElement } from '@app/ui/fields/text';
import { buttonCls } from '@app/ui/style';
import { useCountries } from '@app/ui/use-countries';
import { useZodForm } from '@/lib/use-schema-form';
import { Edit, Plus, Trash2 } from 'lucide-react';
import React from 'react';
import { useMutation } from 'urql';
import { TypeOf, z } from 'zod';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from './form';
import { SubmitButton } from './submit';
import { useAuth } from './use-auth';

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

export function CreateMembershipApplicationForm({ data, onSuccess }: { data?: MembershipApplicationFragment; onSuccess?: () => void }) {
  const { user } = useAuth();
  const countries = useCountries();
  const { reset, control, handleSubmit, formState: { errors } } = useZodForm(Form);
  const create = useMutation(CreateMembershipApplicationDocument)[1];
  const update = useMutation(UpdateMembershipApplicationDocument)[1];
  const del = useMutation(DeleteMembershipApplicationDocument)[1];

  React.useEffect(() => {
    if (data) {
      reset(Form.partial().optional().parse(data));
    }
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    console.log(values);
    if (data) {
      await update({ input: { id: data.id, patch: values } });
    } else {
      await create({ input: { membershipApplication: { ...values, createdBy: user?.id! } }});
    }
    onSuccess?.();
  });

  return (
    <form className="grid lg:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={errors as any || onSubmit.error} />

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

      <div className="col-full flex justify-between">
        {data && (
          <button type="button" onClick={() => del({ input: { id: data.id } })} className={buttonCls({ variant: 'outline' })}>
            <Trash2 />
            Smazat přihlášku
          </button>
        )}

        <SubmitButton loading={onSubmit.loading} />
      </div>
    </form>
  );
}

export function MembershipApplicationCard({ item }: { item: MembershipApplicationFragment }) {
  const [open, setOpen] = React.useState(false);

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <button className={buttonCls({ variant: 'outline' })}>
          <Edit />
          {item.firstName} {item.lastName}
        </button>
      </DialogTrigger>
      <DialogContent>
        <CreateMembershipApplicationForm data={item} onSuccess={() => setOpen(false)} />
      </DialogContent>
    </Dialog>
  );
};

export function CreateMembershipApplicationButton() {
  const [open, setOpen] = React.useState(false);

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <button className={buttonCls({ variant: 'outline' })}>
          <Plus />
          Přihláška nového člena
        </button>
      </DialogTrigger>
      <DialogContent>
        <CreateMembershipApplicationForm onSuccess={() => setOpen(false)} />
      </DialogContent>
    </Dialog>
  );
};
