import { UpdateTenantDocument } from '@/graphql/Tenant';
import { useZodForm } from '@/lib/use-schema-form';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { useTenant } from '@/ui/useTenant';
import { Edit } from 'lucide-react';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { z } from 'zod';

const Form = z.object({
  name: z.string(),
  bankAccount: z.string(),
  description: z.string(),
});
type FormProps = z.infer<typeof Form>;

export function EditTenantDialog() {
  const [open, setOpen] = React.useState(false);

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <button className={buttonCls({variant: 'outline'})}>
          <Edit/>
          Upravit
        </button>
      </DialogTrigger>
      <DialogContent>
        <EditTenantForm onSuccess={() => setOpen(false)} />
      </DialogContent>
    </Dialog>
  );
}

function EditTenantForm({ onSuccess }: { onSuccess?: () => void; }) {
  const { data } = useTenant();
  const doUpdate = useMutation(UpdateTenantDocument)[1];

  const { reset, control, handleSubmit } = useZodForm(Form);
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ input: { id: data!.id, patch: values } });
    onSuccess?.();
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error}/>
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
      <SubmitButton loading={onSubmit.loading}/>
    </form>
  );
}
