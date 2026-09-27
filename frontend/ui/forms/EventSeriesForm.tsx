import { UpdateEventSeriesDocument } from '@/graphql/Event';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import { useForm } from 'react-hook-form';
import { useMutation } from 'urql';
import { z } from 'zod';

const Form = z.object({ name: z.string() });

export function EventSeriesForm({
  series,
}: {
  series: { id: string; name?: string | null };
}) {
  const { onSuccess } = useFormResult();
  const [result, update] = useMutation(UpdateEventSeriesDocument);
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { name: series.name ?? '' },
  });

  const onSubmit = async ({ name }: z.infer<typeof Form>) => {
    const saved = await update({
      id: series.id,
      patch: { name: name.trim() || null },
    });
    if (!saved.error) onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />
      <TextFieldElement control={control} name="name" label="Název série" />
      <SubmitButton control={control}>Uložit změny</SubmitButton>
    </form>
  );
}
