import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { TenantInput } from 'lib/graphql';
import { useQueryClient } from '@tanstack/react-query';
import {
  TenantFragment,
  useCurrentTenantQuery,
  useUpdateTenantMutation,
} from 'lib/graphql/Tenant';
import { SlateEditorElement } from './Slate';

type FormProps = Pick<TenantInput, 'name' | 'memberInfo'>;

export const TenantForm: React.FC<{
  data: TenantFragment;
}> = ({ data }) => {
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(useCurrentTenantQuery.getKey());
  }, [queryClient]);

  const { mutateAsync: doUpdate } = useUpdateTenantMutation({ onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  const [iter, setIter] = React.useState(0);
  React.useEffect(() => {
    reset({
      name: data?.name,
      memberInfo: data?.memberInfo,
    });
    setIter(x => x + 1);
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ input: { id: data.id, patch: values } });
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="name" label="Název organizace" required />
      <SlateEditorElement
        iter={iter}
        control={control}
        name="memberInfo"
        label="Informace pro členy"
      />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
