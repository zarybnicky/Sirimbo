import {
  type AccessCredentialFragment,
  CreateAccessCredentialDocument,
  UpdateAccessCredentialDocument,
} from '@/graphql/AccessCredential';
import { PersonOptionsDocument } from '@/graphql/Person';
import { TextFieldElement } from '@/ui/fields/text';
import { DatePickerElement } from '@/ui/fields/date';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import React from 'react';

const mifareLabelSchema = z
  .string()
  .trim()
  .superRefine((value, ctx) => {
    if (!/^(0|[1-9][0-9]*)$/.test(value)) {
      ctx.addIssue({
        code: 'custom',
        message: 'Zadejte desítkové UID uvedené na kartě.',
      });
      return;
    }
    if (value.length > 10 || (value.length === 10 && value > '4294967295')) {
      ctx.addIssue({ code: 'custom', message: 'UID musí být 32bitové číslo.' });
    }
  });

const mifareCodeSchema = z
  .string()
  .trim()
  .regex(/^[0-9A-Fa-f]{8}$/, 'UID čtečky musí mít osm hexadecimálních znaků.');

function mifareLabelToCode(label: string): string {
  const value = mifareLabelSchema.parse(label);
  return BigInt(value)
    .toString(16)
    .padStart(8, '0')
    .match(/../g)!
    .toReversed()
    .join('')
    .toUpperCase();
}

export function mifareCodeToLabel(code?: string): string {
  if (!code) return '';
  const value = mifareCodeSchema.parse(code).toUpperCase();
  const hex = value.match(/../g)!.toReversed().join('');
  return BigInt(`0x${hex}`).toString(10);
}

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
  initialCode,
  credential,
}: Readonly<{
  personId?: string;
  initialCode?: string;
  credential?: AccessCredentialFragment;
}>) {
  const { onSuccess } = useFormResult();
  const [{ data }] = useQuery({
    query: PersonOptionsDocument,
    variables: { condition: { membership: { state: 'CURRENT' } } },
    pause: !!credential,
  });
  const { control, handleSubmit, setValue } = useForm({
    resolver: zodResolver(Form),
    defaultValues: {
      personId: credential?.person?.id ?? personId ?? '',
      label: credential?.label ?? mifareCodeToLabel(initialCode) ?? '',
      code: credential?.code ?? initialCode ?? '',
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
      <SubmitButton control={control}>{credential ? 'Uložit' : 'Přidat'}</SubmitButton>
    </form>
  );
}
