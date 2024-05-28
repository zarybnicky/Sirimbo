import {
  EditRegistrationDocument,
  EventFragment,
  EventRegistrationFragment,
  SetLessonDemandDocument
} from '@/graphql/Event';
import { TextAreaElement } from '@/ui/fields/textarea';
import { FormError, useFormResult } from '@/ui/form';
import { formatRegistrant } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { Minus, Plus } from 'lucide-react';
import * as React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';

type FormProps = {
  note: string;
};

export function MyRegistrationForm({ event, registration }: {
  event: EventFragment;
  registration: EventRegistrationFragment;
}) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useForm<FormProps>();
  const edit = useMutation(EditRegistrationDocument)[1];
  const [{ fetching }, setMutation] = useMutation(SetLessonDemandDocument);

  React.useEffect(() => {
    reset({
      note: registration?.note || '',
    });
  }, [reset, registration]);

  const myLessons = React.useMemo(() => {
    const myLessons: Record<string, number> = {};
    registration.eventLessonDemandsByRegistrationIdList.forEach(x => {
      myLessons[x.trainerId] = x.lessonCount;
    });
    return myLessons;
  }, [registration]);

  const changeLessonCount = React.useCallback(
    async (diff: number, trainer: { id: string; lessonsRemaining: number | null; }) => {
      let lessonCount = (myLessons[trainer.id] ?? 0) + diff;
      lessonCount = Math.min(
        lessonCount,
        (myLessons[trainer.id] ?? 0) + (trainer.lessonsRemaining ?? 0)
      );
      lessonCount = Math.max(lessonCount, 0);
      await setMutation({
        input: { registrationId: registration.id, trainerId: trainer.id, lessonCount },
      });
    },
    [setMutation, registration, myLessons]
  );

  const onSubmit = useAsyncCallback(async ({ note }: FormProps) => {
    await edit({ input: { registrationId: registration.id, note } });
    toast.success('Úprava přihlášky proběhla úspěšně.');
    onSuccess();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />
      <b>{formatRegistrant(registration)}</b>

      {(event.enableNotes || !!registration?.note) && (
        <>
          <TextAreaElement
            autoFocus
            control={control}
            label="Poznámky k registraci, požadavky na stravu apod."
            name="note" />
          <SubmitButton loading={onSubmit.loading}>
            Upravit poznámky
          </SubmitButton>
        </>
      )}

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
              {trainer.name}
            </div>
          </div>
        ))}
      </fieldset>
    </form>
  );
}
