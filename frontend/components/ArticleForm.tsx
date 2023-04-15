import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { AktualityInput } from 'lib/graphql';
import {
  ArticleFragment,
  useCreateArticleMutation,
  useUpdateArticleMutation,
} from 'lib/graphql/Articles';
import { SlateEditorElement } from './Slate';

type FormProps = Pick<AktualityInput, 'atJmeno' | 'atPreview' | 'atText'>;

export const ArticleForm: React.FC<{
  data?: ArticleFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateArticleMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateArticleMutation({ onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      atJmeno: data?.atJmeno,
      atPreview: data?.atPreview,
      atText: data?.atText,
    });
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      ...values,
      atText: JSON.stringify(values.atText),
      atKat: '1',
    };
    if (data) {
      await doUpdate({ id: data.id, patch });
    } else {
      await doCreate({ input: patch });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="atJmeno" label="Název" required />
      <SlateEditorElement control={control} name="atPreview" label="Shrnutí" />
      <SlateEditorElement control={control} name="atText" label="Text" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
