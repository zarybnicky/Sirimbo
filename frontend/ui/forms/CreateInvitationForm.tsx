import { CreateInvitationDocument } from '@/graphql/Invitation';
import { PersonDocument } from '@/graphql/Person';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { DialogTitle } from '@/ui/dialog';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import React from 'react';

const Form = z.object({
  email: z.email(),
});

export function CreateInvitationForm({ personId }: { personId: string }) {
  const [query] = useQuery({ query: PersonDocument, variables: { id: personId } });
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, reset } = useForm({
    resolver: zodResolver(Form),
  });
  const [result, createInvitation] = useMutation(CreateInvitationDocument);

  React.useEffect(() => {
    if (!query.data?.person) return;
    reset(
      { email: query.data.person.email ?? '' },
      {
        keepDirtyValues: true,
        keepTouched: true,
      },
    );
  }, [query.data?.person, reset]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const result = await createInvitation({
      input: { personInvitation: { personId, email: values.email } },
    });
    if (!result.error) onSuccess();
  };

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Pozvat e-mailem</DialogTitle>
      <FormError error={query.error || result.error} />
      <TextFieldElement
        control={control}
        name="email"
        label="E-mail, kam poslat pozvánku"
      />
      <SubmitButton control={control} />
    </form>
  );
}
