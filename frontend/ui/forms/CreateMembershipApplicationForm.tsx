import {
  ConfirmMembershipApplicationDocument,
  CreateMembershipApplicationDocument,
  DeleteMembershipApplicationDocument,
  type MembershipApplicationFragment,
  UpdateMembershipApplicationDocument,
} from '@/graphql/CurrentUser';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { DatePickerElement } from '@/ui/fields/date';
import { TextFieldElement } from '@/ui/fields/text';
import { CstsIdFieldElement } from '@/ui/fields/CstsIdFieldElement';
import { FormError, useFormResult } from '@/ui/form';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { useAuth } from '@/lib/auth';
import { countryOptions } from '@/lib/countries';
import { parseCzechBirthNumber } from '@/lib/czechBirthNumber';
import { Check, Trash2 } from 'lucide-react';
import React from 'react';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { sanitizeUnicode } from '@/lib/sanitize';

const Form = z.object({
  prefixTitle: z.string().prefault('').overwrite(sanitizeUnicode),
  firstName: z.string({ error: 'Zadejte jméno' }).min(1, 'Zadejte jméno').overwrite(sanitizeUnicode),
  lastName: z.string({ error: 'Zadejte příjmení' }).min(1, 'Zadejte příjmení').overwrite(sanitizeUnicode),
  suffixTitle: z.string().prefault('').overwrite(sanitizeUnicode),
  gender: z.enum(['MAN', 'WOMAN', 'UNSPECIFIED'], { error: 'Vyberte pohlaví' }),
  birthDate: z.string().nullish(),
  email: z.email().nullish(),
  phone: z.string().min(9).max(14).nullish(),
  cstsId: z.number().int().positive().nullable().optional(),
  wdsfId: z.number().int().positive().nullable().optional(),
  taxIdentificationNumber: z
    .string()
    .regex(/^(?:\d{9,10})?$/, 'Neplatné rodné číslo')
    .nullish(),
  nationality: z.string(),
  bio: z.string().prefault(''),
});

export function CreateMembershipApplicationForm({
  data,
  onCreate,
}: {
  data?: MembershipApplicationFragment;
  onCreate?: (id: string) => void;
}) {
  const { onSuccess } = useFormResult();
  const auth = useAuth();
  const { reset, control, handleSubmit, getValues, setValue } = useForm({
    resolver: zodResolver(Form),
  });

  const fillBirthDate = () => {
    if (getValues('birthDate')) return;
    const birthDate = parseCzechBirthNumber(getValues('taxIdentificationNumber'));
    if (birthDate) setValue('birthDate', birthDate, { shouldDirty: true });
  };
  const [createResult, create] = useMutation(CreateMembershipApplicationDocument);
  const [updateResult, update] = useMutation(UpdateMembershipApplicationDocument);
  const [confirmResult, confirm] = useMutation(ConfirmMembershipApplicationDocument);
  const [deleteResult, del] = useMutation(DeleteMembershipApplicationDocument);

  const disabled = auth.isAdmin && !!data;

  React.useEffect(() => {
    if (data) {
      reset(Form.partial().optional().parse(data), {
        keepDirtyValues: true,
        keepTouched: true,
        keepErrors: true,
      });
    }
  }, [reset, data]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (!auth.user) return;

    let result;
    let createdId: string | undefined;
    if (data) {
      result = await update({ input: { id: data.id, patch: values } });
    } else {
      result = await create({
        input: {
          membershipApplication: {
            ...values,
            createdBy: auth.user.id,
          },
        },
      });
      createdId = result.data?.createMembershipApplication?.membershipApplication?.id;
    }
    if (!result.error) {
      onSuccess();
      if (createdId) onCreate?.(createdId);
    }
  };

  const onConfirm = async () => {
    if (!data) return;
    const result = await confirm({ input: { applicationId: data.id } });
    if (!result.error) onSuccess();
  };

  const onDelete = async () => {
    if (!data) return;
    const result = await del({ input: { id: data.id } });
    if (!result.error) onSuccess();
  };

  return (
    <form onSubmit={handleSubmit(onSubmit)}>
      <fieldset className="grid lg:grid-cols-2 gap-2" disabled={disabled}>
        <FormError
          error={
            createResult.error ||
            updateResult.error ||
            confirmResult.error ||
            deleteResult.error
          }
        />

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

        <TextFieldElement
          control={control}
          name="email"
          type="email"
          label="E-mail"
          autoComplete="email"
        />
        <TextFieldElement
          control={control}
          name="phone"
          type="tel"
          label="Telefon"
          autoComplete="tel"
        />

        <DatePickerElement
          control={control}
          label="Datum narození"
          name="birthDate"
          valueMode="date"
        />
        <TextFieldElement
          control={control}
          name="taxIdentificationNumber"
          label="Rodné číslo"
          placeholder="1111119999"
          inputMode="numeric"
          onBlur={fillBirthDate}
        />

        <CstsIdFieldElement control={control} name="cstsId" />
        <TextFieldElement
          control={control}
          name="wdsfId"
          type="number"
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

        <div className="col-full">
          <ComboboxElement
            control={control}
            label="Národnost"
            name="nationality"
            placeholder="vyberte národnost"
            options={countryOptions}
          />
        </div>
      </fieldset>

      <div className="col-full flex justify-between">
        {data && auth.isAdmin ? (
          <>
            <button className={buttonCls()} type="button" onClick={onConfirm}>
              <Check />
              Potvrdit jako člena
            </button>

            <button
              className={buttonCls({ variant: 'outline' })}
              type="button"
              onClick={onDelete}
            >
              <Trash2 />
              Smazat přihlášku
            </button>
          </>
        ) : (
          <>
            {data && (
              <button
                type="button"
                onClick={onDelete}
                className={buttonCls({ variant: 'outline' })}
              >
                <Trash2 />
                Smazat přihlášku
              </button>
            )}

            <SubmitButton control={control} disabled={disabled} />
          </>
        )}
      </div>
    </form>
  );
}
