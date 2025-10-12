import { UpdateTenantDocument } from '@/graphql/Tenant';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useTenant } from '@/ui/useTenant';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  name: z.string(),
  bankAccount: z.string(),
  description: z.string(),
});

export function EditTenantForm() {
  const { data } = useTenant();
  const { onSuccess } = useFormResult();
  const doUpdate = useMutation(UpdateTenantDocument)[1];

  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    if (!data) return;
    await doUpdate({ input: { id: data.id, patch: values } });
    onSuccess();
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error}/>
      <TextFieldElement
        control={control}
        name="name"
        label="Název organizace"
        required
      />
      <TextFieldElement
        control={control}
        name="bankAccount"
        label="Číslo účtu"
        required
      />
      <RichTextEditor
        control={control}
        initialState={data?.description}
        name="description"
        label="Základní informace"
      />
      <SubmitButton loading={onSubmit.loading}/>
    </form>
  );
}
