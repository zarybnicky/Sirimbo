import { zodResolver } from '@hookform/resolvers/zod';
import { CreateUserProxyDocument } from '@/graphql/Memberships';
import { UserListDocument } from '@/graphql/CurrentUser';
import { CreateInvitationDocument } from '@/graphql/Invitation';
import { PersonMembershipsDocument } from '@/graphql/Person';
import { DialogTitle } from '@/ui/dialog';
import { ComboboxSearchArea } from '@/ui/fields/Combobox';
import { FieldHelper, FieldLabel, FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useController, useForm } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { Mail } from 'lucide-react';

const Form = z.object({
  email: z
    .string()
    .trim()
    .pipe(z.email({ error: 'Zadejte platný e-mail.' })),
});

export function AddPersonAccessForm({ personId }: { personId: string }) {
  const { onSuccess } = useFormResult();
  const [selectedUserId, setSelectedUserId] = React.useState<string | null>(null);
  const {
    control,
    handleSubmit,
    reset,
    setError,
    formState: { errors, isSubmitting },
  } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { email: '' },
  });

  const [list] = useQuery({ query: UserListDocument });
  const [query] = useQuery({
    query: PersonMembershipsDocument,
    variables: { id: personId },
  });
  const [, createUserProxy] = useMutation(CreateUserProxyDocument);
  const [, createInvitation] = useMutation(CreateInvitationDocument);
  const person = query.data?.person;
  const users = list.data?.users?.nodes ?? [];

  React.useEffect(() => {
    if (!person) return;
    const { email, userProxiesList } = person;
    const alreadyLinked = userProxiesList.some(
      (x) =>
        x.status !== 'EXPIRED' && x.user?.uEmail.toLowerCase() === email?.toLowerCase(),
    );
    reset(
      { email: alreadyLinked ? '' : (email ?? '') },
      { keepDirtyValues: true, keepTouched: true },
    );
  }, [person, reset]);

  const { field, fieldState } = useController({ control, name: 'email' });
  const email = field.value.trim().toLowerCase();
  const linkedIds = new Set(
    (person?.userProxiesList ?? [])
      .filter((x) => x.status !== 'EXPIRED')
      .map((x) => x.user?.id),
  );
  const matches = users.filter((x) => x.uEmail.toLowerCase() === email);
  const selected =
    matches.find((x) => x.id === selectedUserId) ??
    (matches.length === 1 ? matches[0] : undefined);
  const validEmail = Form.shape.email.safeParse(field.value).success;
  const suggestions =
    matches.length > 0
      ? matches.filter((x) => !linkedIds.has(x.id))
      : email
        ? users
            .filter(
              (x) => !linkedIds.has(x.id) && x.uEmail.toLowerCase().startsWith(email),
            )
            .slice(0, 5)
        : [];
  const busy = query.fetching || list.fetching || isSubmitting;

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (matches.length > 1 && !selected) {
      setError('email', {
        message: 'E-mail odpovídá více účtům. Vyberte účet z nabídky.',
      });
      return;
    }
    if (selected && linkedIds.has(selected.id)) {
      setError('email', { message: 'Tento uživatel už má přístup.' });
      return;
    }
    const response = selected
      ? await createUserProxy({
          input: { userProxy: { personId, userId: selected.id } },
        })
      : await createInvitation({
          input: { personInvitation: { personId, email: values.email } },
        });
    if (response.error) {
      setError('root.server', { message: response.error.message });
    } else {
      onSuccess();
    }
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Pozvat uživatele</DialogTitle>
      <FormError error={query.error || list.error || errors.root?.server?.message} />
      <FieldLabel>E-mail</FieldLabel>
      <ComboboxSearchArea
        inline
        inputIcon={Mail}
        value={selected?.id}
        onChange={(id) => {
          const user = users.find((x) => x.id === id);
          setSelectedUserId(user?.id ?? null);
          field.onChange(user?.uEmail ?? '');
        }}
        options={suggestions.map((x) => ({
          id: x.id,
          label: [x.uEmail, x.uLogin].filter(Boolean).join(', '),
        }))}
        emptyMessage={email && !validEmail ? 'Zadejte platný e-mail.' : null}
        disabled={busy}
        inputProps={{
          name: field.name,
          type: 'email',
          required: true,
          ref: field.ref,
          value: field.value,
          onValueChange: (value) => {
            setSelectedUserId(null);
            field.onChange(value);
          },
          onBlur: field.onBlur,
          placeholder: 'jmeno@example.com',
          'aria-label': 'E-mail',
          'aria-invalid': !!fieldState.error,
        }}
      />
      <FieldHelper error={fieldState.error} />
      <SubmitButton
        control={control}
        disabled={!validEmail || busy || !!query.error || !!list.error || !person}
      >
        Pozvat
      </SubmitButton>
    </form>
  );
}
