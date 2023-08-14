import {
  EventFragment,
  RegisterToEventDocument,
} from '@app/graphql/Event';
import { useAuth } from '@app/ui/use-auth';
import * as React from 'react';
import { buttonCls } from './style/button';
import { useForm } from 'react-hook-form';
import { TextAreaElement } from '@app/ui/fields/textarea';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { Plus } from 'lucide-react';
import { ComboboxElement } from './Combobox';
import {
  formatCoupleName,
} from '@app/ui/format-name';
import { RegisterToEventInput } from '@app/graphql';
import { cn } from './cn';

type FormProps = {
  participant: string;
  note: string;
  lessons: { trainerId: string; lessonCount: number; }[];
};

export const NewRegistrationForm = ({ event }: { event: EventFragment }) => {
  const create = useMutation(RegisterToEventDocument)[1];
  const [mode, setMode] = React.useState<'view' | 'edit'>('view');
  const { persons, couples } = useAuth();
  const { control, handleSubmit } = useForm<FormProps>({ defaultValues: { lessons: [], note: '' } });

  const possibleParticipants = React.useMemo(() => {
    const possibleCouples = couples.map((c) => ({ id: `couple-${c.id}`, label: formatCoupleName(c) }));
    const possiblePersons = persons.map((p) => ({
      id: `person-${p.id}`,
      label: `${p.firstName} ${p.lastName}`,
    }));
    return possibleCouples.concat(possiblePersons);
  }, [event, persons, couples]);

  const onSubmit = useAsyncCallback(async ({ participant, lessons, note }: FormProps) => {
    const [type, id] = participant.split('-');
    const registration: RegisterToEventInput['registration'] =
      type === 'couple'
        ? { eventId: event.id, note: note || '', coupleId: id }
        : { eventId: event.id, note: note || '', personId: id };
    await create({ input: { registration, lessons } });
    toast.success('Přihlášení na akci proběhlo úspěšně.');
    setMode('view');
  });

  return mode === 'view' ? (
    <button type="button" className={buttonCls()} onClick={() => setMode('edit')}>
      <Plus /> Přihlásit
    </button>
  ) : (
    <form onSubmit={handleSubmit(onSubmit.execute)}>
      <fieldset
        className={cn(
          'group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 m-1',
          'space-y-2',
        )}
      >
        <legend className="bg-white text-neutral-11 text-sm">Nová přihláška</legend>
        <FormError error={onSubmit.error} />
        <ComboboxElement
          control={control}
          name="participant"
          placeholder="Vyberte účastníka"
          options={possibleParticipants}
        />
        {event.enableNotes ? (
          <TextAreaElement
            autoFocus
            control={control}
            label="Požadavky na lekce, stravu apod."
            name="note"
          />
        ) : null}
        <SubmitButton loading={onSubmit.loading}>Přihlásit</SubmitButton>
      </fieldset>
    </form>
  );
};
