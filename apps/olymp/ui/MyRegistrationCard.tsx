import {
  EventFragment,
  EventRegistrationFragment,
  SetLessonDemandDocument,
  EditRegistrationDocument,
  CancelRegistrationDocument,
} from '@app/graphql/Event';
import * as React from 'react';
import { buttonCls } from '@app/ui/style';
import { useForm } from 'react-hook-form';
import { TextAreaElement } from '@app/ui/fields/textarea';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { Minus, Plus } from 'lucide-react';
import { formatFullName, formatRegistrant } from '@app/ui/format';
import { Card } from './Card';
import { Dialog, DialogContent } from './dialog';

type FormProps = {
  note: string;
};

export const MyRegistrationCard = ({
  event,
  registration,
}: {
  event: EventFragment;
  registration: EventRegistrationFragment;
}) => {
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
    (diff: number, trainer: { id: string; lessonsRemaining: number | null }) => {
      let lessonCount = (myLessons[trainer.id] ?? 0) + diff;
      lessonCount = Math.min(
        lessonCount,
        (myLessons[trainer.id] ?? 0) + (trainer.lessonsRemaining ?? 0),
      );
      lessonCount = Math.max(lessonCount, 0);
      setMutation({
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
    await cancel({ input: { registrationId: registration.id! } });
    toast.success('Přihláška zrušena úspěšně.');
    setOpen(false);
  });

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <Card>
        <b>{formatRegistrant(registration)}</b>
        {registration.statusTime}
        {registration.isConfirmed}
        {registration.paymentId}
        Požadavky na lekce
        {registration.eventLessonDemandsByRegistrationIdList.map((x) => (
          <div key={x.id}>
            {formatFullName(
              event.eventTrainersList.find((t) => t.id === x.trainerId)?.person!,
            )}
            {': '}
            {x.lessonCount}
          </div>
        ))}
        {registration.note}
        <button className={buttonCls({variant: 'outline'})} onClick={() => setOpen(true)}>
          Upravit přihlášku
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
                label="Požadavky na lekce, stravu apod."
                name="note"
              />
              <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
            </>
          ) : null}

          <div>
            <button
              type="button"
              className={buttonCls({ variant: 'outline' })}
              onClick={onCancel.execute}
            >
              Zrušit přihlášku
            </button>
          </div>

          <fieldset>
            <legend>Požadavky na lekce</legend>
            {event.eventTrainersList.map((trainer) => (
              <div key={trainer.id}>
                <button
                  className="text-accent-9 disabled:text-accent-7"
                  onClick={() => changeLessonCount(-1, trainer)}
                  disabled={fetching || !myLessons[trainer.id]}
                >
                  <Minus className="w-5 h-5" />
                </button>
                <div className="text-xl tabular-nums">{myLessons[trainer.id] ?? 0}x</div>
                <button
                  className="text-accent-9 disabled:text-accent-7"
                  onClick={() => changeLessonCount(1, trainer)}
                  disabled={fetching || (trainer.lessonsRemaining ?? 0) < 1}
                >
                  <Plus className="w-5 h-5" />
                </button>
              </div>
            ))}
          </fieldset>
        </form>
      </DialogContent>
    </Dialog>
  );
};