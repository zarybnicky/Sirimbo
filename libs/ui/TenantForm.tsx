import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import {CurrentTenantDocument, UpdateTenantDocument} from '@app/graphql/Tenant';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { RichTextEditor } from '@app/editor/RichTextEditor';
import { TitleBar } from './TitleBar';

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
    reset(Form.partial().optional().parse(data));
    // TODO: increment richtexteditor key
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ input: { id: data!.id, patch: values } });
  });

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar title={data?.name || '(Bez názvu)'}>
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>
      <FormError error={onSubmit.error} />
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
