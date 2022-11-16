import { VideoSourceFragment, useCreateVideoSourceMutation, useUpdateVideoSourceMutation } from 'lib/graphql/Video';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { VideoSourceInput } from 'lib/graphql';

type FormProps = Pick<VideoSourceInput, 'vsUrl' | 'vsTitle' | 'vsDescription'>;

export const VideoSourceForm: React.FC<{
  data?: VideoSourceFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateVideoSourceMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateVideoSourceMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      vsUrl: data?.vsUrl,
      vsTitle: data?.vsTitle,
      vsDescription: data?.vsDescription,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (!data) {
      await doCreate({ input: values });
    } else {
      await doUpdate({ id: data.vsId, patch: values });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="vsUrl" required
        label={<> ID kanálu (např. <code>https://www.youtube.com/channel/<b>UCopG139AfgpmaswNXmEwX2Q</b></code>)</>}
        helperText="Ostatní pole se později zjistí z YouTube, pokud zůstanou prázdná."
      />
      <TextFieldElement control={control} name="vsTitle" label="Jméno" required />
      <TextFieldElement control={control} name="vsDescription" label="Popis" required />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
