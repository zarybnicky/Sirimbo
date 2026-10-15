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
    defaultValues: { userId: '' },
  });
  const [list] = useQuery({ query: UserListDocument });
  const [query] = useQuery({
    query: PersonMembershipsDocument,
    variables: { id: personId },
  });
  const [result, create] = useMutation(CreateUserProxyDocument);
  const owner = query.data?.person;
  const options = React.useMemo(() => {
    if (!owner) return [];
    const linkedIds = new Set(
      (owner.userProxiesList ?? [])
        .filter((x) => x.status !== 'EXPIRED')
        .map((x) => x.user?.id),
    );
    return (list.data?.users?.nodes ?? [])
      .filter((item) => !linkedIds.has(item.id))
      .map((item) => ({ id: item.id, label: [item.uEmail, item.uLogin].filter(isTruthy).join(', ') }));
  }, [owner, list.data]);
  const fetching = query.fetching || list.fetching;
  const queryError = query.error || list.error;

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (fetching || queryError || !options.some((x) => x.id === values.userId)) {
      setError('userId', { message: 'Vyberte dostupnou položku ze seznamu.' });
      return;
    }
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
      <FormError error={result.error || queryError} />
      <ComboboxElement
        control={control}
        name="userId"
        options={options}
        label="Uživatelský účet"
        placeholder="Vyberte účet"
      />
      <SubmitButton
        control={control}
        disabled={fetching || !!queryError || options.length === 0}
      >
        Přiřadit účet
      </SubmitButton>
    </form>
  );
}
