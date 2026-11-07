import { deleteEventRecursiveAction } from '@/lib/event-actions';
import { DeleteEventDocument, EventWithAttendanceDocument } from '@/graphql/Event';
import { DialogTitle } from '@/ui/dialog';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { usePathname, useRouter } from 'next/navigation';
import { useForm } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';

export function DeleteEventForm({ id, name }: { id: string; name: string }) {
  const { onSuccess } = useFormResult();
  const router = useRouter();
  const pathname = usePathname();
  const [query] = useQuery({ query: EventWithAttendanceDocument, variables: { id } });
  const [, deleteEvent] = useMutation(DeleteEventDocument);
  const {
    control,
    handleSubmit,
    setError,
    formState: { errors },
  } = useForm({
    defaultValues: { acknowledged: false },
  });

  const onSubmit = async (values: { acknowledged: boolean }) => {
    const response = values.acknowledged
      ? await deleteEventRecursiveAction(id)
      : await deleteEvent({ id });
    if (response.error) {
      setError('root.server', {
        message:
          typeof response.error === 'string' ? response.error : response.error.message,
      });
      return;
    }
    onSuccess();
    if (pathname === `/termin/${id}`) router.replace('/akce');
  };

  return (
    <form className="space-y-3" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Odstranit událost</DialogTitle>
      <p>
        Událost <strong>{name}</strong> bude trvale odstraněna. Odstraní se také
        přihlášky, docházka, požadavky na lekce, platby apod.
      </p>
      <FormError error={errors.root?.server?.message ?? query.error} />
      {!!query.data?.event?.childEventsList.length && (
        <CheckboxElement
          className="my-2"
          control={control}
          name="acknowledged"
          label="Odstranit i všechny vnořené události a jejich data?"
        />
      )}
      <SubmitButton control={control} disabled={!query.data?.event}>
        Odstranit
      </SubmitButton>
    </form>
  );
}
