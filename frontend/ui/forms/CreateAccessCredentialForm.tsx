import { CreateAccessCredentialDocument } from '@/graphql/AccessCredential';
import { TextFieldElement } from '@/ui/fields/text';
import { DatePickerElement } from '@/ui/fields/date';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z
  .object({
    uid: z.string().trim().min(1, 'Vyplňte UID karty nebo tokenu.'),
    label: z.string(),
    since: z.date(),
    until: z.date().nullish(),
  });

export function CreateAccessCredentialForm({ personId }: { personId: string }) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { uid: '', label: '', since: new Date(), until: null },
  });
  const [result, create] = useMutation(CreateAccessCredentialDocument);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const result = await create({
      input: {
        accessCredential: {
          personId,
          uid: values.uid,
          label: values.label,
          since: values.since.toISOString(),
          until: values.until?.toISOString() ?? null,
        },
      },
    });
    if (!result.error) onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />
      <TextFieldElement control={control} name="uid" label="UID karty / tokenu" />
      <TextFieldElement control={control} name="label" label="Popis (volitelný)" />
      <DatePickerElement control={control} name="since" label="Platné od" />
      <DatePickerElement
        control={control}
        name="until"
        label="Platné do"
        clearable
      />
      <SubmitButton control={control}>Přidat kartu</SubmitButton>
    </form>
  );
}
