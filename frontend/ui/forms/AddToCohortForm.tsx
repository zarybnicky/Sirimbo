import { CreateCohortMembershipDocument } from '@/graphql/Memberships';
import type { PersonFragment } from '@/graphql/Person';
import { FormError, useFormResult } from '@/ui/form';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { VerticalCheckboxButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { SubmitButton } from '@/ui/submit';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { CohortListDocument } from '@/graphql/Cohorts';

const Form = z.object({
  cohortIds: z.array(z.string()).min(1, 'Vyberte alespoň jednu skupinu'),
});

export function AddToCohortForm({ person }: { person: PersonFragment }) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  const [result, createCohortMember] = useMutation(CreateCohortMembershipDocument);

  const [{ data: cohorts }] = useQuery({
    query: CohortListDocument,
    variables: { archived: false },
  });
  const cohortOptions = React.useMemo(
    () => cohorts?.cohortsList?.map((x) => ({ id: x.id, label: x.name })) || [],
    [cohorts],
  );

  const onSubmit = async (values: z.infer<typeof Form>) => {
    for (const cohortId of values.cohortIds) {
      const result = await createCohortMember({
        input: { cohortMembership: { personId: person.id, cohortId } },
      });
      if (result.error) return;
    }
    onSuccess();
  };

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />
      <VerticalCheckboxButtonGroupElement
        control={control}
        name="cohortIds"
        options={cohortOptions}
      />
      <SubmitButton control={control} />
    </form>
  );
}
