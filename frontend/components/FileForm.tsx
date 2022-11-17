import React from 'react';
import { FileFragment, useUpdateFileMutation } from 'lib/graphql/Documents';
import { DokumentyInput } from 'lib/graphql';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<DokumentyInput, 'dName'>;

export const FileForm: React.FC<{
  data: FileFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doUpdate } = useUpdateFileMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      dName: data?.dName,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ id: data.id, patch: values });
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="dName" label="Název" required />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
