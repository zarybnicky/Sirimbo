import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { TenantInput } from 'lib/graphql';
import { useQueryClient } from '@tanstack/react-query';
import {
  CurrentTenantDocument,
  TenantFragment,
  UpdateTenantDocument,
} from 'lib/graphql/Tenant';
import dynamic from 'next/dynamic';
import { pipe } from 'fp-ts/lib/function';
import { pick } from 'lib/form-utils';
import { getGqlKey, useGqlMutation } from 'lib/query';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });

const fields = ['name', 'memberInfo'] as const;
type FormProps = Pick<TenantInput, (typeof fields)[number]>;

export const TenantForm: React.FC<{
  data: TenantFragment;
}> = ({ data }) => {
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(getGqlKey(CurrentTenantDocument, {}));
  }, [queryClient]);

  const { mutateAsync: doUpdate } = useGqlMutation(UpdateTenantDocument, { onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    if (data) {
      reset(pipe(data, pick(fields)));
    }
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ input: { id: data.id, patch: values } });
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="name" label="Název organizace" required />
      <RichTextEditor
        control={control}
        initialState={data?.memberInfo}
        name="memberInfo"
        label="Informace pro členy"
      />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
