import { ConfirmMembershipApplicationDocument, CreateMembershipApplicationDocument, DeleteMembershipApplicationDocument, type MembershipApplicationFragment, UpdateMembershipApplicationDocument } from '@/graphql/CurrentUser';
import { useZodForm } from '@/lib/use-schema-form';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { useAuth } from '@/ui/use-auth';
import { useCountries } from '@/ui/use-countries';
import { Check, Trash2 } from 'lucide-react';
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
});

export function CreateMembershipApplicationForm({ data }: {
  disabled?: boolean;
  data?: MembershipApplicationFragment;
}) {
  const { onSuccess } = useFormResult();
  const auth = useAuth();
  const countries = useCountries();
  const { reset, control, handleSubmit, formState: { errors } } = useZodForm(Form);
  const create = useMutation(CreateMembershipApplicationDocument)[1];
  const update = useMutation(UpdateMembershipApplicationDocument)[1];
  const confirm = useMutation(ConfirmMembershipApplicationDocument)[1];
  const del = useMutation(DeleteMembershipApplicationDocument)[1];

  const disabled = auth.isAdmin;

  React.useEffect(() => {
    if (data) {
      reset(Form.partial().optional().parse(data));
    }
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    if (data) {
      await update({ input: { id: data.id, patch: values } });
    } else {
      await create({ input: { membershipApplication: { ...values, createdBy: auth.user?.id! } }});
    }
    onSuccess();
  });

  return (
    <form onSubmit={handleSubmit(onSubmit.execute)}>
      <fieldset className="grid lg:grid-cols-2 gap-2" disabled={disabled}>
        <FormError error={errors as any || onSubmit.error} />

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
      </fieldset>

      <div className="col-full flex justify-between">
        {(data && auth.isAdmin) ? (
          <>
            <button className={buttonCls()} type="button" onClick={async () => {
              await confirm({ input: { applicationId: data.id } })
              onSuccess();
            }}>
              <Check />
              Potvrdit jako člena
            </button>

            <button className={buttonCls({ variant: 'outline' })} type="button" onClick={async () => {
              await del({ input: { id: data.id } })
              onSuccess();
            }}>
              <Trash2 />
              Smazat přihlášku
            </button>
          </>
        ) : (
          <>
            {data && (
              <button type="button" onClick={() => del({ input: { id: data.id } })} className={buttonCls({ variant: 'outline' })}>
                <Trash2 />
                Smazat přihlášku
              </button>
            )}

            <SubmitButton loading={onSubmit.loading} />
          </>
        )}
      </div>
    </form>
  );
}
