import {
  type AccessCredentialFragment,
  AccessCredentialPeopleDocument,
  CreateAccessCredentialDocument,
  UpdateAccessCredentialDocument,
} from '@/graphql/AccessCredential';
import { TextFieldElement } from '@/ui/fields/text';
import { DatePickerElement } from '@/ui/fields/date';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import {
  mifareCodeSchema,
  mifareCodeToLabel,
  mifareLabelSchema,
  mifareLabelToCode,
} from '@/lib/access-credentials';
import React from 'react';

const Form = z
  .object({
    personId: z.string().min(1, 'Vyberte osobu'),
    label: mifareLabelSchema,
    code: mifareCodeSchema,
    since: z.date(),
    until: z.date().nullish(),
  })
  .superRefine((value, ctx) => {
    const label = mifareLabelSchema.safeParse(value.label);
    const code = mifareCodeSchema.safeParse(value.code);
    if (label.success && code.success && mifareLabelToCode(label.data) !== code.data) {
      ctx.addIssue({
        code: 'custom',
        path: ['code'],
        message: 'UID karty a čtečky neodpovídají.',
      });
    }
  });

export function AccessCredentialForm({
  personId,
  initialValue,
  credential,
}: Readonly<{
  personId?: string;
  initialValue?: {
    label: string;
    code: string;
  };
  credential?: AccessCredentialFragment;
}>) {
  const { onSuccess } = useFormResult();
  const [{ data }] = useQuery({
    query: AccessCredentialPeopleDocument,
    pause: !!credential,
  });
  const { control, handleSubmit, setValue } = useForm({
    resolver: zodResolver(Form),
    defaultValues: {
      personId: credential?.person?.id ?? personId ?? '',
      label: credential?.label ?? initialValue?.label ?? '',
      code: credential?.code ?? initialValue?.code ?? '',
      since: credential ? new Date(credential.since) : new Date(),
      until: credential?.until ? new Date(credential.until) : null,
    },
  });
  const [createResult, create] = useMutation(CreateAccessCredentialDocument);
  const [updateResult, update] = useMutation(UpdateAccessCredentialDocument);
  const people = React.useMemo(
    () => data?.people?.nodes.map((x) => ({ id: x.id, label: x.name })) ?? [],
    [data?.people?.nodes],
  );

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const value = {
      label: values.label,
      code: values.code,
      since: values.since.toISOString(),
      until: values.until?.toISOString() ?? null,
    };
    const result = credential
      ? await update({ id: credential.id, patch: value })
      : await create({
          input: {
            accessCredential: {
              ...value,
              personId: values.personId,
              kind: 'MIFARE',
            },
          },
        });
    if (!result.error) onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={createResult.error ?? updateResult.error} />
      {!credential && (
        <ComboboxElement
          control={control}
          name="personId"
          label="Osoba"
          placeholder="Vyberte osobu"
          options={people}
        />
      )}
      <TextFieldElement
        control={control}
        name="label"
        label="ID uvedené na kartě"
        inputMode="numeric"
        onChange={(event) => {
          const label = mifareLabelSchema.safeParse(event.currentTarget.value);
          setValue('code', label.success ? mifareLabelToCode(label.data) : '', {
            shouldValidate: true,
          });
        }}
      />
      <TextFieldElement
        control={control}
        name="code"
        label="UID ze čtečky"
        inputClassName="font-mono uppercase"
        onChange={(event) => {
          event.currentTarget.value = event.currentTarget.value.toUpperCase();
          const code = mifareCodeSchema.safeParse(event.currentTarget.value);
          if (code.success) {
            setValue('label', mifareCodeToLabel(code.data), {
              shouldValidate: true,
            });
          }
        }}
      />
      <DatePickerElement control={control} name="since" label="Platné od" />
      <DatePickerElement control={control} name="until" label="Platné do" clearable />
      <SubmitButton control={control}>
        {credential ? 'Uložit změny' : 'Přidat kartu'}
      </SubmitButton>
    </form>
  );
}
