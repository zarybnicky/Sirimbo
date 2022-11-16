import React from 'react';
import { VideoInput } from 'lib/graphql';
import { VideoFragment, useCreateVideoMutation, useUpdateVideoMutation } from 'lib/graphql/Video';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from './TextField';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<VideoInput, 'vUri' | 'vTitle' | 'vDescription' | 'vPlaylist' | 'vAuthor'>;

export const VideoForm: React.FC<{
  data?: VideoFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateVideoMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateVideoMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      vUri: data?.vUri,
      vTitle: data?.vTitle,
      vDescription: data?.vDescription,
      vPlaylist: data?.vPlaylist,
      vAuthor: data?.vAuthor,
    },
  });

  // $split = preg_split("/(vi\/|v=|\/v\/|youtu\.be\/|\/embed\/)/", $uri);
  // $query = ($split && isset($split[1])) ? $split[1] : $uri;
  // $query = preg_split("/[^0-9a-z_\-]/i", $query);
  // return $query[0];

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (!data) {
      await doCreate({ input: values });
    } else {
      await doUpdate({ id: data.vId, patch: values });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="vUri" required label={<>
        ID videa (např. <code>https://www.youtube.com/watch?v=<b>lt6h7Fgxohs</b></code>)<br />
        Ostatní pole se později zjistí z YouTube, pokud zůstanou prázdná.
      </>} />
      <TextFieldElement control={control} name="vTitle" label="Jméno" required />
      <TextFieldElement control={control} name="vAuthor" label="Autor/kanál" required />
      <TextFieldElement control={control} name="vPlaylist" label="ID playlistu (nepovinné)" required />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
