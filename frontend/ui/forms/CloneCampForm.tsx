import { cloneCampAction } from '@/lib/event-actions';
import { DialogTitle } from '@/ui/dialog';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import { addWeeks, format } from 'date-fns';
import { useForm } from 'react-hook-form';
import { z } from 'zod';

const Form = z.object({
  since: z.string().min(1, 'Vyberte začátek soustředění.'),
  copyCohorts: z.boolean(),
  copyTrainers: z.boolean(),
});

export function CloneCampForm({ id, since }: { id: string; since: string }) {
  const { control, handleSubmit, setError, formState: { errors } } = useForm({
    resolver: zodResolver(Form),
    defaultValues: {
      since: format(addWeeks(new Date(since), 1), "yyyy-MM-dd'T'HH:mm"),
      copyCohorts: true,
      copyTrainers: true,
    },
  });

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const result = await cloneCampAction(id, {
      ...values, since: new Date(values.since).toISOString(),
    });
    setError('root', { message: result.error });
  };

  return (
    <form className="space-y-3" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Duplikovat soustředění</DialogTitle>
      <FormError error={errors.root?.message} />
      <TextFieldElement control={control} name="since" type="datetime-local" label="Nový začátek" />
      <CheckboxElement control={control} name="copyCohorts" label="Přihlásit aktuální členy skupin" />
      <CheckboxElement control={control} name="copyTrainers" label="Zkopírovat trenéry" />
      <SubmitButton control={control}>Duplikovat</SubmitButton>
    </form>
  );
}
