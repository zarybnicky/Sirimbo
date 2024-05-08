import { CreateCohortMembershipDocument } from "@/graphql/Memberships";
import { PersonFragment } from "@/graphql/Person";
import { useZodForm } from "@/lib/use-schema-form";
import React from "react";
import { useAsyncCallback } from "react-async-hook";
import { useMutation } from "urql";
import { TypeOf, z } from "zod";
import { VerticalCheckboxButtonGroupElement } from "./RadioButtomGroupElement";
import { SubmitButton } from "./submit";
import { useCohorts } from "./useCohorts";

const Form = z.object({
  cohortIds: z.array(z.string()),
});

export function AddToCohortForm({ person, onSuccess }: { person: PersonFragment; onSuccess?: () => void }) {
  const { control, handleSubmit } = useZodForm(Form);
  const createCohortMember = useMutation(CreateCohortMembershipDocument)[1];

  const { data: cohorts } = useCohorts({ visible: true });
  const cohortOptions = React.useMemo(() => cohorts.map(x => ({id: x.id, label: x.name})), [cohorts]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    for (const cohortId of values.cohortIds) {
      await createCohortMember({ input: { cohortMembership: { personId: person.id, cohortId } } })
    }
    onSuccess?.();
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
