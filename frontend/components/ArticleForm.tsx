import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { AktualityInput } from 'lib/graphql';
import {
  ArticleFragment,
  CreateArticleDocument,
  UpdateArticleDocument,
} from 'lib/graphql/Articles';
import dynamic from 'next/dynamic';
import { useGqlMutation } from 'lib/query';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });

type FormProps = Pick<AktualityInput, 'atJmeno' | 'atPreview' | 'atText'>;

export const ArticleForm: React.FC<{
  data?: ArticleFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useGqlMutation(CreateArticleDocument, { onSuccess });
  const { mutateAsync: doUpdate } = useGqlMutation(UpdateArticleDocument, { onSuccess });

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
      <RichTextEditor
        control={control}
        initialState={data?.atPreview}
        name="atPreview"
        label="Shrnutí"
      />
      <RichTextEditor
        control={control}
        initialState={data?.atText}
        name="atText"
        label="Text"
      />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
