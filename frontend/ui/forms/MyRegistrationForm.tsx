import {
  EditRegistrationDocument,
  type EventFragment,
  type EventRegistrationFragment,
  SetLessonDemandDocument,
} from '@/graphql/Event';
import { TextAreaElement } from '@/ui/fields/textarea';
import { FormError, useFormResult } from '@/ui/form';
import { formatRegistrant } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { Minus, Plus } from 'lucide-react';
import * as React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  note: z.string().prefault(''),
});

type FormValues = z.infer<typeof Form>;

export function MyRegistrationForm({
  event,
  registration,
}: {
  event: EventFragment;
  registration: EventRegistrationFragment;
}) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  const edit = useMutation(EditRegistrationDocument)[1];
  const [{ fetching }, setMutation] = useMutation(SetLessonDemandDocument);

  React.useEffect(() => {
    reset(
      {
        note: registration?.note || '',
      },
      {
        keepDirtyValues: true,
        keepTouched: true,
        keepErrors: true,
      },
    );
  }, [reset, registration]);

  const myLessons = React.useMemo(() => {
    const myLessons: Record<string, number> = {};
    for (const x of registration.eventLessonDemandsByRegistrationIdList) {
      myLessons[x.trainerId] = x.lessonCount;
    }
    return myLessons;
  }, [registration]);
  const lessonTrainers = event.type === 'CAMP' || event.eventInstancesList.length === 1
    ? event.eventTrainersList.filter(
        (trainer) =>
          (trainer.lessonsOffered as number | null) !== 0 ||
          (myLessons[trainer.id] ?? 0) > 0,
      )
    : [];

  const changeLessonCount = React.useCallback(
    async (
      diff: number,
      trainer: {
        id: string;
        lessonsOffered: number | null;
        lessonsRemaining: number | null;
      },
    ) => {
      const currentLessonCount = myLessons[trainer.id] ?? 0;
      let lessonCount = currentLessonCount + diff;
      if (diff > 0 && trainer.lessonsOffered !== null) {
        lessonCount = Math.min(
          lessonCount,
          currentLessonCount + Math.max(trainer.lessonsRemaining ?? 0, 0),
        );
      }
      lessonCount = Math.max(lessonCount, 0);
      await setMutation({
        input: { registrationId: registration.id, trainerId: trainer.id, lessonCount },
      });
    },
    [setMutation, registration, myLessons],
  );

  const onSubmit = useAsyncCallback(async ({ note }: FormValues) => {
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
            name="note"
          />
          <SubmitButton loading={onSubmit.loading}>Upravit poznámky</SubmitButton>
        </>
      )}

      {lessonTrainers.length > 0 && (
        <fieldset>
          <legend>Požadavky na lekce</legend>
          {lessonTrainers.map((trainer) => {
            const lessonsOffered = trainer.lessonsOffered as number | null;
            const canAddLesson =
              lessonsOffered === null ||
              (lessonsOffered > 0 && (trainer.lessonsRemaining ?? 0) > 0);

            return (
              <div key={trainer.id} className="flex flex-wrap gap-2">
                <button
                  type="button"
                  className="text-accent-9 disabled:text-accent-7"
                  onClick={() => changeLessonCount(-1, trainer)}
                  disabled={fetching || !myLessons[trainer.id]}
                >
                  <Minus className="size-5" />
                </button>
                <div className="text-xl tabular-nums">
                  {myLessons[trainer.id] ?? 0}x
                </div>
                <button
                  type="button"
                  className="text-accent-9 disabled:text-accent-7"
                  onClick={() => changeLessonCount(1, trainer)}
                  disabled={fetching || !canAddLesson}
                >
                  <Plus className="size-5" />
                </button>
                <div className="grow">{trainer.name}</div>
                {lessonsOffered === null && (
                  <div className="text-sm text-neutral-10">bez omezení</div>
                )}
              </div>
            );
          })}
        </fieldset>
      )}
    </form>
  );
}
