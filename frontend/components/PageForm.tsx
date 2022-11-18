import React from 'react';
import { PageFragment, useCreatePageMutation, usePageListQuery, useUpdatePageMutation } from 'lib/graphql/Page';
import { PageInput } from 'lib/graphql';
import { Controller, useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { createValue, Value } from '@react-page/editor';
import { HeadingPlugin } from 'components/Heading';
import { ContainerPlugin } from 'components/Container';
import { CallToActionPlugin } from 'components/CallToAction';
import { ReactPage, cellPlugins } from 'components/ReactPage';
import { useQueryClient } from '@tanstack/react-query';

type FormProps = Pick<PageInput, 'title' | 'content' | 'url'>;

export const PageForm: React.FC<{ data?: PageFragment; }> = ({ data }) => {
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(usePageListQuery.getKey());
  }, []);

  const { mutateAsync: doCreate } = useCreatePageMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdatePageMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      title: data?.title,
      content: data?.content,
      url: data?.url,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.id, patch: values });
    } else {
      await doCreate({ input: values })
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="url" label="URL" required />
      <TextFieldElement control={control} name="title" label="Název" required />
      <Controller
        name="content"
        defaultValue={INITIAL_VALUE}
        render={({ field: { value, onChange } }) => (
          <ReactPage value={value} onChange={onChange} />
        )}
      />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};

const INITIAL_VALUE: Value = createValue({
  rows: [
    [{ plugin: HeadingPlugin }],
    [{ plugin: ContainerPlugin }],
    [{ plugin: CallToActionPlugin }],
  ],
}, {
  cellPlugins,
  lang: 'default',
});
