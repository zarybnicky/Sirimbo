import { CreateCoupleDocument } from '@/graphql/Memberships';
import { type PersonBasicFragment, PersonListDocument } from '@/graphql/Person';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  man: z.string().min(1, 'Vyberte partnera'),
  woman: z.string().min(1, 'Vyberte partnerku'),
});

export function CreateCoupleForm({
  person: initialPerson,
}: {
  person?: PersonBasicFragment;
}) {
  const { onSuccess } = useFormResult();
  const [result, doCreate] = useMutation(CreateCoupleDocument);
  const [{ data }] = useQuery({ query: PersonListDocument });
  const men = React.useMemo(
    () =>
      (data?.people?.nodes || [])
        .filter((x) => x.gender === 'MAN')
        .toSorted((a, b) =>
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
      (data?.people?.nodes || [])
        .filter((x) => x.gender === 'WOMAN')
        .toSorted((a, b) =>
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
    mode: 'onBlur',
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    if (initialPerson && initialPerson.gender === 'MAN') {
      reset({ man: initialPerson.id });
    } else if (initialPerson && initialPerson.gender === 'WOMAN') {
      reset({ woman: initialPerson.id });
    }
  }, [initialPerson, reset]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const result = await doCreate({
      input: {
        couple: {
          manId: values.man,
          womanId: values.woman,
          since: new Date().toISOString(),
        },
      },
    });
    if (!result.error && result.data?.createCouple?.couple?.id) onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />
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
      <SubmitButton control={control}>Spárovat</SubmitButton>
    </form>
  );
}
