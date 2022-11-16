import { RoleFragment, useCreateRoleMutation, useUpdateRoleMutation } from 'lib/graphql/Roles';
import React from 'react';
import { useForm } from 'react-hook-form';
import { useAsyncCallback } from 'react-async-hook'
import { permissionLabels, realPermissionKeys } from "lib/data/use-permissions";
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { SubmitButton } from './SubmitButton';
import { PermissionSlider } from './PermissionSlider';
import { PermissionInput } from 'lib/graphql';

type FormProps = PermissionInput;

export const RoleForm: React.FC<{
  data?: RoleFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateRoleMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateRoleMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: data,
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.peId, patch: values });
    } else {
      await doCreate({
        input: { ...values, peAnkety: 1, peInzerce: 1, peKonzole: 1 },
      });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TextFieldElement control={control} name="peName" label="Jméno" required />
      <TextAreaElement control={control} name="peDescription" label="Popis" rows={3} required />
      {realPermissionKeys.map(key => (
        <PermissionSlider control={control} key={key} name={key} label={permissionLabels[key]} />
      ))}
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}
