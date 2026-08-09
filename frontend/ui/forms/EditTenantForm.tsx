import { CurrentTenantDocument, UpdateTenantDocument } from '@/graphql/Tenant';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  name: z.string(),
  bankAccount: z.string(),
  description: z.string(),
});

export function EditTenantForm() {
  const [{ data }] = useQuery({ query: CurrentTenantDocument });
  const { onSuccess } = useFormResult();
  const [result, update] = useMutation(UpdateTenantDocument);

  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data?.tenant), {
      keepDirtyValues: true,
      keepTouched: true,
      keepErrors: true,
    });
  }, [reset, data?.tenant]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (!data) return;
    const result = await update({ input: { id: data.tenant?.id!, patch: values } });
    if (!result.error) onSuccess();
  };

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />
      <TextFieldElement control={control} name="name" label="Název organizace" required />
      <TextFieldElement
        control={control}
        name="bankAccount"
        label="Číslo účtu"
        required
      />
      <RichTextEditor
        control={control}
        initialState={data?.tenant?.description}
        name="description"
        label="Základní informace"
      />
      <SubmitButton control={control} />
    </form>
  );
}
