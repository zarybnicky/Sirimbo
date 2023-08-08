import { RichTextView } from '@app/ui/RichTextView';
import { EventDocument, EventFragment, EventRegistrationFragment, SetLessonDemandDocument } from '@app/graphql/Event';
import { Card } from '@app/ui/Card';
import { fullDateFormatter } from '@app/ui/format-date';
import { useAuth } from '@app/ui/use-auth';
import * as React from 'react';
import { EventParticipantExport } from './EventParticipantExport';
import { useQuery } from 'urql';
import { buttonCls } from './style/button';
import { RegisterToEventDocument, CancelRegistrationDocument } from '@app/graphql/Event';
import { useForm } from 'react-hook-form';
import { TextAreaElement } from '@app/ui/fields/textarea';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { Minus, Plus } from 'lucide-react';
import { ComboboxElement } from './Combobox';
import { formatCoupleName } from '@app/ui/format-name';
import { RegisterToEventInput } from '@app/graphql';

export const EventItem = ({ id }: { id: string }) => {
  const { user, perms } = useAuth();
  const [{ data }] = useQuery({ query: EventDocument, variables:{ id }, pause: !id });
  const event = data?.event;
  const registrations = event?.eventRegistrationsList || [];
  const myRegistrations = registrations.filter(x => perms.isCurrentCouple(x.coupleId) || perms.isCurrentPerson(x.personId));

  if (!event) return null;

  const total = event.eventRegistrationsList?.length ?? 0;
  return (
    <Card className="break-inside-avoid">
      <div className="flex justify-between flex-wrap text-neutral-11">
        <div>
          {event.eventInstancesList.map(item => fullDateFormatter.formatRange(new Date(item.range.start!.value), new Date(item.range.end!.value)))}
        </div>
        {parseInt(event.capacity) > 0 && (
          <div>
            Zbývá {event.remainingPersonSpots} míst z {event.capacity}
          </div>
        )}
    {event.eventTrainersList.reduce((n, x) => n + (x.lessonsRemaining || 0), 0) > 0 && (
          <div>
            Zbývá {event.remainingLessons} z {event.eventTrainersList.reduce((n, x) => n + (x.lessonsRemaining || 0), 0)} lekcí
          </div>
        )}
      </div>
      <div className="text-2xl text-neutral-12">{event.name}</div>
      <div className="text-neutral-11">{event.locationText}</div>

      {myRegistrations.map(reg => <RegistrationForm key={reg.id} event={event} registration={reg} />)}

      {total > 0 && (
        <Card>
          <h3>Účastníci</h3>
          {perms.isTrainerOrAdmin && <EventParticipantExport id={event.id} />}
          {event.eventRegistrationsList?.map((x) => (
            <div key={x.person?.id}>
              {x.person?.firstName} {x.person?.lastName}
            </div>
          ))}
        </Card>
      )}

      <RichTextView value={event.summary} />
      <RichTextView value={event.description} />
      {!!user && (
        <RichTextView value={event.descriptionMember} />
      )}
    </Card>
  );
};

type FormProps = {
  participant: string;
  note: string;
}

export const RegistrationForm = ({ event, registration }: {
  event: EventFragment;
  registration: EventRegistrationFragment;
}) => {
  const { persons, couples } = useAuth();

  const create = useMutation(RegisterToEventDocument)[1];
  const cancel = useMutation(CancelRegistrationDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      note: registration?.note || '',
    });
  }, [reset, registration]);

  const onSubmit = useAsyncCallback(async ({ participant, note }: FormProps) => {
    if (event) {
      const [type, id] = participant.split('-');
      const input: RegisterToEventInput = type === 'couple' ? { eventId: event.id, note, coupleId: id } : { eventId: event.id, note, personId: id };
      await create({input});
      toast.success('Přihlášení na akci proběhla úspěšně.');
    }
  });
  const onCancel = useAsyncCallback(async () => {
    if (registration) {
      await cancel({ input: { registrationId: registration.id! } });
      toast.success('Přihláška zrušena úspěšně.');
    }
  });

  return (
        <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
          <FormError error={onSubmit.error} />
      <ComboboxElement
        control={control}
        name="participant"
        label="Účastník"
        placeholder="Vyberte účastníka"
        options={
        persons.map(p => ({id: `person-${p.id}`, label: `${p.firstName} ${p.lastName}`}))
               .concat(couples.map(c => ({ id: `coupleId-${c.id}`, label: formatCoupleName(c)})))
        }
      />
          {event.enableNotes || registration?.note ? (
            <TextAreaElement
              autoFocus
              control={control}
              label="Požadavky na lekce, stravu apod."
              name="note"
            />
          ) : null}
          <SubmitButton loading={onSubmit.loading}>
            {registration ? 'Upravit přihlášku' : 'Přihlásit'}
          </SubmitButton>
          {registration && (
            <button type="button" className={buttonCls({ variant: 'outline' })} onClick={onCancel.execute}>
              Zrušit přihlášku
            </button>
          )}
        </form>
  );
};

export const LessonDemandForm = ({ event, registration }: {
  event: EventFragment;
  registration: EventRegistrationFragment;
}) => {
  const [{ fetching }, setMutation] = useMutation(SetLessonDemandDocument);
  const trainers = event.eventTrainersList;
  const myLessons = registration.eventLessonDemandsByRegistrationIdList.reduce((xs, x) => {
    xs[x.trainerId] = x.lessonCount;
    return xs;
  }, {} as Record<string, number>);

  const addLesson = React.useCallback((trainer: { id: string; lessonsRemaining: number | null; }) => {
    const lessonCount = Math.min((myLessons[trainer.id] ?? 0) + 1, (myLessons[trainer.id] ?? 0) + (trainer.lessonsRemaining ?? 0));
    setMutation({ input: { registrationId: registration.id, trainerId: trainer.id, lessonCount }});
  }, [setMutation, registration, myLessons]);

  const removeLesson = React.useCallback((trainer: { id: string; }) => {
    const lessonCount = Math.max((myLessons[trainer.id] ?? 0) - 1, 0);
    setMutation({ input: { registrationId: registration.id, trainerId: trainer.id, lessonCount }});
  }, [setMutation, registration, myLessons]);

  return (
    <Card>
      {trainers.map(trainer => (
        <div>
      <button
        className="text-accent-9 disabled:text-accent-7"
        onClick={() => removeLesson(trainer)}
        disabled={fetching || !myLessons[trainer.id]}
      >
        <Minus className="w-5 h-5" />
      </button>
      <div className="text-xl tabular-nums">{myLessons[trainer.id] ?? 0}x</div>
      <button
        className="text-accent-9 disabled:text-accent-7"
        onClick={() => addLesson(trainer)}
        disabled={fetching || (trainer.lessonsRemaining ?? 0) < 1}
      >
        <Plus className="w-5 h-5" />
      </button>
        </div>
      ))}
    </Card >
  );
};
