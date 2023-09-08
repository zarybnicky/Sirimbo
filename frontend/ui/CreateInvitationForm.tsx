import { CreateInvitationDocument } from "@/graphql/Person";
import { PersonFragment } from "@/graphql/Person";
import { useZodForm } from "@/lib/use-schema-form";
import React from "react";
import { useAsyncCallback } from "react-async-hook";
import { useMutation } from "urql";
import { TypeOf, z } from "zod";
import { SubmitButton } from "./submit";
import { TextFieldElement } from "./fields/text";

const Form = z.object({
  email: z.string(),
});

export function CreateInvitationForm({ person, onSuccess }: { person: PersonFragment; onSuccess?: () => void }) {
  const { control, handleSubmit } = useZodForm(Form);
  const createInvitation = useMutation(CreateInvitationDocument)[1];

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await createInvitation({ input: { personInvitation: { personId: person.id, email: values.email }} })
    onSuccess?.();
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TextFieldElement control={control} name="email" label="E-mail, kam poslat pozvánku" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}
