import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { AktualityInput } from 'lib/graphql';
import { ArticleFragment, useCreateArticleMutation, useUpdateArticleMutation } from 'lib/graphql/Articles';

type FormProps = Pick<AktualityInput, 'atJmeno' | 'atPreview' | 'atText'>;

export const ArticleForm: React.FC<{
  data?: ArticleFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateArticleMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateArticleMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      atJmeno: data?.atJmeno,
      atPreview: data?.atPreview,
      atText: data?.atText,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.atId, patch: values });
    } else {
      await doCreate({ input: { ...values, atKat: '1' } });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="atJmeno" label="Název" required />
      <TextAreaElement control={control} name="atPreview" label="Shrnutí" rows={3} required />
      <TextAreaElement control={control} name="atText" label="Text" rows={20} required />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
