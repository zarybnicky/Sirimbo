import { CurrentTenantDocument, UpdateTenantDocument } from '@/graphql/Tenant';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { useZodForm } from '@/lib/use-schema-form';
import { Edit } from 'lucide-react';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const Form = z.object({
  name: z.string(),
  bankAccount: z.string(),
  description: z.string(),
});
type FormProps = z.infer<typeof Form>;

export function EditTenantDialog({ onSuccess }: { onSuccess?: () => void }) {
  const [open, setOpen] = React.useState(false);
  const [query] = useQuery({ query: CurrentTenantDocument });
  const doUpdate = useMutation(UpdateTenantDocument)[1];
  const data = query.data?.tenant;

  const { reset, control, handleSubmit } = useZodForm(Form);
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ input: { id: data!.id, patch: values } });
    onSuccess?.();
  });

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <button className={buttonCls({ variant: 'outline' })}>
          <Edit />
          Upravit
        </button>
      </DialogTrigger>
      <DialogContent>
        <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
          <FormError error={onSubmit.error} />
          <TextFieldElement
            control={control}
            name="name"
            label="Název organizace"
            required
          />
          <TextFieldElement
            control={control}
            name="bankAccount"
            label="Číslo účtu"
            required
          />
          <RichTextEditor
            control={control}
            initialState={data?.description}
            name="description"
            label="Základní informace"
          />
          <SubmitButton loading={onSubmit.loading} />
        </form>
      </DialogContent>
    </Dialog>
  );
}
