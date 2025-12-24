import { CreateCoupleDocument } from '@/graphql/Memberships';
import { type PersonBasicFragment, PersonListDocument } from '@/graphql/Person';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  man: z.string(),
  woman: z.string(),
});

export function CreateCoupleForm({ initial }: { initial?: PersonBasicFragment }) {
  const { onSuccess } = useFormResult();
  const doCreate = useMutation(CreateCoupleDocument)[1];
  const [{ data }] = useQuery({ query: PersonListDocument });
  const men = React.useMemo(
    () =>
      (data?.filteredPeopleList || [])
        .filter((x) => x.gender === 'MAN')
        .sort((a, b) =>
          `${a.lastName} ${a.firstName}`.localeCompare(`${b.lastName} ${b.firstName}`),
        )
        .map((x) => ({
          id: x.id,
          label:
            x.name + (x.birthDate ? ` (${new Date(x.birthDate).getFullYear()})` : ''),
        })),
    [data],
  );
  const women = React.useMemo(
    () =>
      (data?.filteredPeopleList || [])
        .filter((x) => x.gender === 'WOMAN')
        .sort((a, b) =>
          `${a.lastName} ${a.firstName}`.localeCompare(`${b.lastName} ${b.firstName}`),
        )
        .map((x) => ({
          id: x.id,
          label:
            x.name + (x.birthDate ? ` (${new Date(x.birthDate).getFullYear()})` : ''),
        })),
    [data],
  );

  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    if (initial && initial.gender === 'MAN') {
      reset({ man: initial.id });
    } else if (initial && initial.gender === 'WOMAN') {
      reset({ woman: initial.id });
    }
  }, [initial, reset]);

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    const res = await doCreate({
      input: {
        couple: {
          manId: values.man,
          womanId: values.woman,
          since: new Date().toISOString(),
        },
      },
    });
    const id = res.data?.createCouple?.couple?.id;
    if (id) {
      onSuccess();
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />
      <ComboboxElement
        control={control}
        name="man"
        label="Partner"
        placeholder="vyberte partnera"
        options={men}
      />
      <ComboboxElement
        control={control}
        name="woman"
        label="Partnerka"
        placeholder="vyberte partnerku"
        options={women}
      />
      <SubmitButton loading={onSubmit.loading}>Spárovat</SubmitButton>
    </form>
  );
}
