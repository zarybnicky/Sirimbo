import { zodResolver } from '@hookform/resolvers/zod';
import { CreateUserProxyDocument } from '@/graphql/Memberships';
import { UserListDocument } from '@/graphql/CurrentUser';
import { LinkUserPeopleDocument } from '@/graphql/Person';
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
  personId: z.string().min(1, 'Vyberte osobu'),
});

export function LinkUserToPersonForm({ personId, userId }: {
  personId: string;
  userId?: never;
} | {
  personId?: never;
  userId: string;
}) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { userId: userId ?? '', personId: personId ?? '' },
  });
  const [userQuery] = useQuery({ query: UserListDocument, pause: !!userId });
  const [personQuery] = useQuery({ query: LinkUserPeopleDocument, pause: !!personId });
  const [result, create] = useMutation(CreateUserProxyDocument);
  const userOptions = React.useMemo(
    () =>
      (userQuery.data?.users?.nodes ?? []).map((user) => ({
        id: user.id,
        label: [user.uEmail, user.uLogin].filter(isTruthy).join(', '),
      })),
    [userQuery.data?.users?.nodes],
  );
  const personOptions = React.useMemo(
    () => (personQuery.data?.people?.nodes ?? []).map((person) => ({
      id: person.id,
      label: person.name,
    })),
    [personQuery.data?.people?.nodes],
  );

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const response = await create({
      input: { userProxy: values },
    });
    if (!response.error) onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>{userId ? 'Přiřadit osobu' : 'Přiřadit existující účet'}</DialogTitle>
      <FormError error={result.error || (userId ? personQuery.error : userQuery.error)} />
      <ComboboxElement
        control={control}
        name={userId ? 'personId' : 'userId'}
        options={userId ? personOptions : userOptions}
        label={userId ? 'Osoba' : 'Uživatelský účet'}
        placeholder={userId ? 'Vyberte osobu' : 'Vyberte účet'}
      />
      <SubmitButton control={control}>
        {userId ? 'Přiřadit osobu' : 'Přiřadit účet'}
      </SubmitButton>
    </form>
  );
}
