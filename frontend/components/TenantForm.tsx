import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import {CurrentTenantDocument, UpdateTenantDocument} from 'lib/graphql/Tenant';
import dynamic from 'next/dynamic';
import { useMutation, useQuery } from 'urql';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { Item } from './layout/Item';

const Form = z.object({
  name: z.string(),
  memberInfo: z.string(),
});
type FormProps = z.infer<typeof Form>;

export const TenantForm = () => {
  const [query] = useQuery({query: CurrentTenantDocument});
  const data = query.data?.getCurrentTenant;
  const doUpdate = useMutation(UpdateTenantDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>({
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    reset(Form.optional().parse(data));
    // TODO: increment richtexteditor key
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ input: { id: data!.id, patch: values } });
  });

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <Item.Titlebar title={data?.name || '(Bez názvu)'}>
        <SubmitButton loading={onSubmit.loading} />
      </Item.Titlebar>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="name" label="Název organizace" required />
      <RichTextEditor
        control={control}
        initialState={data?.memberInfo}
        name="memberInfo"
        label="Informace pro členy"
      />
    </form>
  );
};
