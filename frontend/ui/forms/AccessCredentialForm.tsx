import {
  CreateAccessCredentialDocument,
  type AccessCredentialFragment,
  UpdateAccessCredentialDocument,
} from '@/graphql/AccessCredential';
import { TextFieldElement } from '@/ui/fields/text';
import { DatePickerElement } from '@/ui/fields/date';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import {
  mifareCodeSchema,
  mifareCodeToLabel,
  mifareLabelSchema,
  mifareLabelToCode,
} from '@/lib/access-credentials';

const Form = z
  .object({
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
}: {
  personId?: string;
  initialValue?: {
    label: string;
    code: string;
  };
  credential?: AccessCredentialFragment;
}) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, setValue } = useForm({
    resolver: zodResolver(Form),
    defaultValues: {
      label: credential?.label ?? initialValue?.label ?? '',
      code: credential?.code ?? initialValue?.code ?? '',
      since: credential ? new Date(credential.since) : new Date(),
      until: credential?.until ? new Date(credential.until) : null,
    },
  });
  const [createResult, create] = useMutation(CreateAccessCredentialDocument);
  const [updateResult, update] = useMutation(UpdateAccessCredentialDocument);

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
              personId: personId!,
              kind: 'MIFARE',
            },
          },
        });
    if (!result.error) onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={createResult.error ?? updateResult.error} />
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
