import React from "react";
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from "components/ErrorBox";
import { SubmitButton } from "./SubmitButton";
import { useChangePasswordMutation } from "lib/graphql/CurrentUser";

type FormProps = {
  oldPass: string;
  newPass: string;
  checkPass: string;
};

export const ChangePasswordForm: React.FC<{
  onSuccess: () => void;
}> = ({ onSuccess }) => {
  const { mutateAsync: doUpdate } = useChangePasswordMutation({ onSuccess });
  const { control, getValues, handleSubmit } = useForm<FormProps>();
  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ input: { oldPass: values.oldPass, newPass: values.newPass } });
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />

      <TextFieldElement
        control={control} label="Staré heslo" name="oldPass" type="password" autoComplete="current-password" required
      />
      <TextFieldElement
        control={control} label="Nové heslo" name="newPass" type="password" autoComplete="new-password" required
      />
      <TextFieldElement
        control={control} label="Potvrďte nové heslo" name="checkPass" type="password" autoComplete="new-password" required
        validation={{
          validate: (val) => {
            if (val && getValues('newPass') !== val) {
              return 'Nová hesla se neshodují';
            }
          },
        }}
      />

      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
