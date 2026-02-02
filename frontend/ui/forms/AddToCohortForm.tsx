import { CreateCohortMembershipDocument } from '@/graphql/Memberships';
import type { PersonFragment } from '@/graphql/Person';
import { useFormResult } from '@/ui/form';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { VerticalCheckboxButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { SubmitButton } from '@/ui/submit';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { CohortListDocument } from '@/graphql/Cohorts';

const Form = z.object({
  cohortIds: z.array(z.string()),
});

export function AddToCohortForm({ person }: { person: PersonFragment }) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  const createCohortMember = useMutation(CreateCohortMembershipDocument)[1];

  const [{ data: cohorts }] = useQuery({
    query: CohortListDocument,
    variables: { visible: true },
  });
  const cohortOptions = React.useMemo(
    () => cohorts?.cohortsList?.map((x) => ({ id: x.id, label: x.name })) || [],
    [cohorts],
  );

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    for (const cohortId of values.cohortIds) {
      await createCohortMember({
        input: { cohortMembership: { personId: person.id, cohortId } },
      });
    }
    onSuccess();
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <VerticalCheckboxButtonGroupElement
        control={control}
        name="cohortIds"
        options={cohortOptions}
      />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}
