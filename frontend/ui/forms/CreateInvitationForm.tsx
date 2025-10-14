import { CreateInvitationDocument } from "@/graphql/Invitation";
import { type PersonBasicFragment } from "@/graphql/Person";
import { TextFieldElement } from "@/ui/fields/text";
import { useFormResult } from "@/ui/form";
import { SubmitButton } from "@/ui/submit";
import React from "react";
import { useAsyncCallback } from "react-async-hook";
import { useMutation } from "urql";
import { z } from "zod";
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  email: z.string(),
});

export function CreateInvitationForm({ person }: { person: PersonBasicFragment }) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  const createInvitation = useMutation(CreateInvitationDocument)[1];

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    await createInvitation({ input: { personInvitation: { personId: person.id, email: values.email }} })
    onSuccess();
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TextFieldElement control={control} name="email" label="E-mail, kam poslat pozvánku" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}
