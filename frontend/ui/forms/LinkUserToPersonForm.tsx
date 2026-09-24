import { zodResolver } from '@hookform/resolvers/zod';
import { CreateUserProxyDocument } from '@/graphql/Memberships';
import { UserListDocument } from '@/graphql/CurrentUser';
import { isTruthy } from '@/lib/truthyFilter';
import { DialogTitle } from '@/ui/dialog';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useForm } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const Form = z.object({
  userId: z.string().min(1, 'Vyberte uživatelský účet'),
});

export function LinkUserToPersonForm({ person }: { person: { id: string } }) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { userId: '' },
  });
  const [userQuery] = useQuery({ query: UserListDocument });
  const [result, create] = useMutation(CreateUserProxyDocument);
  const userOptions = React.useMemo(
    () =>
      (userQuery.data?.users?.nodes ?? []).map((user) => ({
        id: user.id,
        label: [user.uEmail, user.uLogin].filter(isTruthy).join(', '),
      })),
    [userQuery.data?.users?.nodes],
  );

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const response = await create({
      input: { userProxy: { personId: person.id, userId: values.userId } },
    });
    if (!response.error) onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Přiřadit existující účet</DialogTitle>
      <FormError error={result.error} />
      <ComboboxElement
        control={control}
        name="userId"
        options={userOptions}
        label="Uživatelský účet"
        placeholder="Vyberte účet"
      />
      <SubmitButton control={control}>Přiřadit účet</SubmitButton>
    </form>
  );
}
