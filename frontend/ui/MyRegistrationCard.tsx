import {
  EventFragment,
  EventRegistrationFragment,
  SetLessonDemandDocument,
  EditRegistrationDocument,
  CancelRegistrationDocument,
} from '@/graphql/Event';
import * as React from 'react';
import { buttonCls } from '@/ui/style';
import { useForm } from 'react-hook-form';
import { TextAreaElement } from '@/ui/fields/textarea';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { Minus, Plus } from 'lucide-react';
import { dateTimeFormatter, formatRegistrant } from '@/ui/format';
import { Card } from './Card';
import { Dialog, DialogContent } from './dialog';

type FormProps = {
  note: string;
};

export function MyRegistrationCard({ event, registration }: {
  event: EventFragment;
  registration: EventRegistrationFragment;
}) {
  const [open, setOpen] = React.useState(false);

  const edit = useMutation(EditRegistrationDocument)[1];
  const cancel = useMutation(CancelRegistrationDocument)[1];
  const [{ fetching }, setMutation] = useMutation(SetLessonDemandDocument);

  const myLessons = React.useMemo(() => {
    return (registration.eventLessonDemandsByRegistrationIdList || []).reduce(
      (xs, x) => {
        xs[x.trainerId] = x.lessonCount;
        return xs;
      },
      {} as Record<string, number>,
    );
  }, [registration]);

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      note: registration?.note || '',
    });
  }, [reset, registration]);

  const changeLessonCount = React.useCallback(
    async (diff: number, trainer: { id: string; lessonsRemaining: number | null }) => {
      let lessonCount = (myLessons[trainer.id] ?? 0) + diff;
      lessonCount = Math.min(
        lessonCount,
        (myLessons[trainer.id] ?? 0) + (trainer.lessonsRemaining ?? 0),
      );
      lessonCount = Math.max(lessonCount, 0);
      await setMutation({
        input: { registrationId: registration.id, trainerId: trainer.id, lessonCount },
      });
    },
    [setMutation, registration, myLessons],
  );

  const onSubmit = useAsyncCallback(async ({ note }: FormProps) => {
    await edit({ input: { registrationId: registration.id, note } });
    toast.success('Úprava přihlášky proběhla úspěšně.');
    setOpen(false);
  });

  const onCancel = useAsyncCallback(async () => {
    await cancel({ input: { registrationId: registration.id } });
    toast.success('Přihláška zrušena úspěšně.');
    setOpen(false);
  });

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <Card className="prose prose-accent">
        <h5>{formatRegistrant(registration)}</h5>
        <div>
          Přihlášeno{' '}
          {registration.statusTime === 'REGULAR' ? 'včas ' : ' '}
          {' v '}
          {dateTimeFormatter.format(new Date(registration.createdAt))}
        </div>
        <div>
          {registration.isConfirmed ? 'Přihláška potvrzena' : 'Ještě nepotvrzena'}
        </div>
        {registration.eventLessonDemandsByRegistrationIdList.length > 0 && (
          <div>
            <h5>Požadavky na lekce</h5>
            <ul>
              {registration.eventLessonDemandsByRegistrationIdList.map((x) => (
                <li key={x.id}>
                  {event.eventTrainersList.find((t) => t.id === x.trainerId)?.person?.name}
                  {': '}
                  {x.lessonCount}
                </li>
              ))}
            </ul>
          </div>
        )}
        {registration.note && <p>{registration.note}</p>}

        {event.type !== 'LESSON' && (
          <button className={buttonCls({ variant: 'outline' })} onClick={() => setOpen(true)}>
            Upravit přihlášku
          </button>
        )}

        <button
          type="button"
          className={buttonCls({ variant: 'outline' })}
          onClick={onCancel.execute}
        >
          Zrušit přihlášku
        </button>
      </Card>

      <DialogContent>
        <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
          <FormError error={onSubmit.error} />
          <b>{formatRegistrant(registration)}</b>

          {event.enableNotes || registration?.note ? (
            <>
              <TextAreaElement
                autoFocus
                control={control}
                label="Poznámky k registraci, požadavky na stravu apod."
                name="note"
              />
              <SubmitButton loading={onSubmit.loading}>
                Upravit poznámky
              </SubmitButton>
            </>
          ) : null}

          <fieldset>
            <legend>Požadavky na lekce</legend>
            {event.eventTrainersList.map((trainer) => (
              <div key={trainer.id} className="flex flex-wrap gap-2">
                <button
                  className="text-accent-9 disabled:text-accent-7"
                  onClick={() => changeLessonCount(-1, trainer)}
                  disabled={fetching || !myLessons[trainer.id]}
                >
                  <Minus className="size-5" />
                </button>
                <div className="text-xl tabular-nums">{myLessons[trainer.id] ?? 0}x</div>
                <button
                  className="text-accent-9 disabled:text-accent-7"
                  onClick={() => changeLessonCount(1, trainer)}
                  disabled={fetching || (trainer.lessonsRemaining ?? 0) < 1}
                >
                  <Plus className="size-5" />
                </button>
                <div className="grow">
                  {trainer.person?.name}
                </div>
              </div>
            ))}
          </fieldset>
        </form>
      </DialogContent>
    </Dialog>
  );
};
