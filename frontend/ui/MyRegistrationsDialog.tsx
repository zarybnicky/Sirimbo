import { EventWithRegistrationsFragment, RegisterToEventDocument } from '@app/graphql/Event';
import { useAuth } from '@app/ui/use-auth';
import * as React from 'react';
import { buttonCls } from '@app/ui/style';
import { useForm } from 'react-hook-form';
import { TextAreaElement } from '@app/ui/fields/textarea';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { Plus } from 'lucide-react';
import { ComboboxElement } from './Combobox';
import { formatCoupleName } from '@app/ui/format';
import { RegisterToEventInput } from '@app/graphql';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from './dialog';
import { MyRegistrationCard } from './MyRegistrationCard';

type FormProps = {
  participant: string;
  note: string;
  lessons: { trainerId: string; lessonCount: number }[];
};

function NewRegistrationForm({ event, onSuccess }: {
  event: EventWithRegistrationsFragment;
  onSuccess?: () => void;
}) {
  const create = useMutation(RegisterToEventDocument)[1];
  const { persons, couples } = useAuth();

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: { lessons: [], note: '' },
  });

  const possibleParticipants = React.useMemo(() => {
    let possibleParticipants = persons.map((p) => ({
      id: `person-${p.id}`,
      label: `${p.firstName} ${p.lastName}`,
    }));

    if (event.capacity == 0 || (event.remainingPersonSpots ?? 0) > 1) {
      possibleParticipants = possibleParticipants.concat(
        couples.filter(x => x.active).map((c) => ({
          id: `couple-${c.id}`,
          label: formatCoupleName(c),
        }))
      );
    }

    return possibleParticipants;
  }, [persons, couples]);

  const onSubmit = useAsyncCallback(async ({ participant, lessons, note }: FormProps) => {
    const [type, id] = participant.split('-');
    const registration: RegisterToEventInput['registration'] =
      type === 'couple'
        ? { eventId: event.id, note: note || '', coupleId: id }
        : { eventId: event.id, note: note || '', personId: id };
    const res = await create({ input: { registration, lessons } });
    const newId = res.data?.registerToEvent?.registration?.id;
    if (newId) {
      toast.success('Přihlášení na akci proběhlo úspěšně.');
      onSuccess?.();
    }
  });

  if (event.capacity > 0 && !event.remainingPersonSpots) {
    return null;
  }

  return (
    <form onSubmit={handleSubmit(onSubmit.execute)} className="space-y-2">
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
    </form>
  );
}

export function MyRegistrationsDialog({ event }: { event: EventWithRegistrationsFragment }) {
  const [open, setOpen] = React.useState(false);
  const { perms } = useAuth();

  const registrations = event?.eventRegistrationsList || [];
  const myRegistrations = registrations.filter(
    (x) => perms.isCurrentCouple(x.coupleId) || perms.isCurrentPerson(x.personId),
  );

  if (
    event.isLocked ||
    event.eventInstancesList.every(i => new Date(i.since) < new Date()) ||
    (event.capacity > 0 && (event.remainingPersonSpots ?? 0) <= 0 && myRegistrations.length == 0)
  ) {
    return null;
  }

  return (
    <Dialog open={open} onOpenChange={setOpen} modal={false}>
      <DialogTrigger asChild>
        <button className={buttonCls()}>
          {myRegistrations.length > 0 ? (
            <>Moje přihlášky</>
          ) : (
            <><Plus /> Přihlásit</>
          )}
        </button>
      </DialogTrigger>

      <DialogContent>
        <DialogTitle>Moje přihlášky</DialogTitle>

        {myRegistrations.map((reg) => (
          <MyRegistrationCard key={reg.id} event={event} registration={reg} />
        ))}

        <NewRegistrationForm event={event} onSuccess={() => setOpen(false)} />
      </DialogContent>
    </Dialog>
  );
};
