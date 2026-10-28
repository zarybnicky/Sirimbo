import { zodResolver } from '@hookform/resolvers/zod';
import { CreateUserProxyDocument } from '@/graphql/Memberships';
import { UserDetailDocument } from '@/graphql/CurrentUser';
import { PersonOptionsDocument } from '@/graphql/Person';
import { DialogTitle } from '@/ui/dialog';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useForm } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const Form = z.object({
  personId: z.string().min(1, 'Vyberte osobu'),
});

export function LinkPersonToUserForm({ userId }: { userId: string }) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, setError } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { personId: '' },
  });
  const [list] = useQuery({
    query: PersonOptionsDocument,
    variables: { condition: null },
  });
  const [query] = useQuery({
    query: UserDetailDocument,
    variables: { id: userId },
  });
  const [result, create] = useMutation(CreateUserProxyDocument);
  const owner = query.data?.user;
  const options = React.useMemo(() => {
    if (!owner) return [];
    const linkedIds = new Set(
      (owner.userProxiesList ?? [])
        .filter((x) => x.status !== 'EXPIRED')
        .map((x) => x.person?.id),
    );
    return (list.data?.people?.nodes ?? [])
      .filter((item) => !linkedIds.has(item.id))
      .map((item) => ({ id: item.id, label: item.name }));
  }, [owner, list.data]);
  const fetching = query.fetching || list.fetching;
  const queryError = query.error || list.error;

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (fetching || queryError || !options.some((x) => x.id === values.personId)) {
      setError('personId', { message: 'Vyberte dostupnou položku ze seznamu.' });
      return;
    }
    const response = await create({
      input: { userProxy: { userId, ...values } },
    });
    if (response.error) {
      setError('root.server', { message: response.error.message });
      return;
    }
    onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Přiřadit osobu</DialogTitle>
      <FormError error={result.error || queryError} />
      <ComboboxElement
        inline
        control={control}
        name="personId"
        label="Osoba"
        placeholder="Vyberte osobu"
        options={options}
        disabled={result.fetching}
      />
      <SubmitButton
        control={control}
        disabled={fetching || !!queryError || options.length === 0}
      >
        Přiřadit osobu
      </SubmitButton>
    </form>
  );
}
