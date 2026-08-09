import { CreateInvitationDocument } from '@/graphql/Invitation';
import { type PersonBasicFragment } from '@/graphql/Person';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  email: z.email(),
});

export function CreateInvitationForm({ person }: { person: PersonBasicFragment }) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useForm({
    mode: 'onBlur',
    resolver: zodResolver(Form),
  });
  const [result, createInvitation] = useMutation(CreateInvitationDocument);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const result = await createInvitation({
      input: { personInvitation: { personId: person.id, email: values.email } },
    });
    if (!result.error) onSuccess();
  };

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />
      <TextFieldElement
        control={control}
        name="email"
        label="E-mail, kam poslat pozvánku"
      />
      <SubmitButton control={control} />
    </form>
  );
}
