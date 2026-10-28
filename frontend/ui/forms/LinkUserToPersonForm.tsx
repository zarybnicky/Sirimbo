import { zodResolver } from '@hookform/resolvers/zod';
import { CreateUserProxyDocument } from '@/graphql/Memberships';
import { UserListDocument } from '@/graphql/CurrentUser';
import { PersonMembershipsDocument } from '@/graphql/Person';
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

export function LinkUserToPersonForm({ personId }: { personId: string }) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, setError } = useForm({
    resolver: zodResolver(Form),
  });

  const [list] = useQuery({ query: UserListDocument });
  const [query] = useQuery({
    query: PersonMembershipsDocument,
    variables: { id: personId },
  });
  const [result, create] = useMutation(CreateUserProxyDocument);

  const options = React.useMemo(() => {
    if (!query.data?.person) return [];
    const linkedIds = new Set(
      (query.data?.person.userProxiesList ?? [])
        .filter((x) => x.status !== 'EXPIRED')
        .map((x) => x.user?.id),
    );
    return (list.data?.users?.nodes ?? [])
      .filter((item) => !linkedIds.has(item.id))
      .map((item) => ({ id: item.id, label: [item.uEmail, item.uLogin].filter(isTruthy).join(', ') }));
  }, [query.data?.person, list.data]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const response = await create({
      input: { userProxy: { personId, ...values } },
    });
    if (response.error) {
      setError('root.server', { message: response.error.message });
      return;
    }
    onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Přiřadit existující účet</DialogTitle>
      <FormError error={result.error || query.error || list.error} />
      <ComboboxElement
        inline
        control={control}
        name="userId"
        label="Uživatelský účet"
        placeholder="Vyberte účet"
        options={options}
        disabled={result.fetching}
      />
      <SubmitButton control={control}>
        Přiřadit účet
      </SubmitButton>
    </form>
  );
}
