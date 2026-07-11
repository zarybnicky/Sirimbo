import {
  type EventInstanceRegistrationFragment,
  type EventInstanceTrainerLessonOfferFragment,
  type EventLessonDemandFragment,
  SetEventInstanceRegistrationDocument,
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
  instanceId,
  enableNotes,
  registration,
  lessonTrainers,
}: {
  instanceId: string;
  enableNotes: boolean;
  registration: EventInstanceRegistrationFragment;
  lessonTrainers: EventInstanceTrainerLessonOfferFragment[];
}) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  const edit = useMutation(SetEventInstanceRegistrationDocument)[1];

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

  const onSubmit = useAsyncCallback(async ({ note }: FormValues) => {
    const result = await edit({
      input: {
        pInstanceId: instanceId,
        pPersonId: registration.personId,
        pCoupleId: registration.coupleId,
        pIsRegistered: true,
        pNote: note,
      },
    });
    if (result.error) throw result.error;
    toast.success('Úprava přihlášky proběhla úspěšně.');
    onSuccess();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />
      <b>{formatRegistrant(registration)}</b>

      {(enableNotes || !!registration.note) && (
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

      <LessonDemandControls
        registrationId={registration.id}
        demands={registration.eventLessonDemandsByRegistrationIdList}
        trainers={lessonTrainers}
      />
    </form>
  );
}

function LessonDemandControls({
  registrationId,
  demands,
  trainers,
}: {
  registrationId: string;
  demands: EventLessonDemandFragment[];
  trainers: EventInstanceTrainerLessonOfferFragment[];
}) {
  const [{ fetching, error }, setMutation] = useMutation(SetLessonDemandDocument);
  const myLessons = Object.fromEntries(
    demands.map((demand) => [demand.trainerId, demand.lessonCount]),
  );
  const lessonTrainers = trainers.filter(
    (trainer) => trainer.lessonsOffered !== 0 || (myLessons[trainer.id] ?? 0) > 0,
  );

  if (lessonTrainers.length === 0) return null;

  return (
    <fieldset>
      <legend>Požadavky na lekce</legend>
      <FormError error={error} />
      {lessonTrainers.map((trainer) => {
        const currentLessonCount = myLessons[trainer.id] ?? 0;
        const canAddLesson =
          trainer.lessonsOffered === null ||
          (trainer.lessonsOffered > 0 && (trainer.lessonsRemaining ?? 0) > 0);

        const changeLessonCount = async (diff: number) => {
          const available = Math.max(trainer.lessonsRemaining ?? 0, 0);
          const lessonCount = Math.max(
            0,
            trainer.lessonsOffered === null
              ? currentLessonCount + diff
              : Math.min(currentLessonCount + diff, currentLessonCount + available),
          );
          await setMutation({
            input: {
              instanceRegistrationId: registrationId,
              instanceTrainerId: trainer.id,
              lessonCount,
            },
          });
        };

        return (
          <div key={trainer.id} className="flex flex-wrap gap-2">
            <button
              type="button"
              className="text-accent-9 disabled:text-accent-7"
              onClick={() => changeLessonCount(-1)}
              disabled={fetching || currentLessonCount === 0}
            >
              <Minus className="size-5" />
            </button>
            <div className="text-xl tabular-nums">{currentLessonCount}x</div>
            <button
              type="button"
              className="text-accent-9 disabled:text-accent-7"
              onClick={() => changeLessonCount(1)}
              disabled={fetching || !canAddLesson}
            >
              <Plus className="size-5" />
            </button>
            <div className="grow">{trainer.person?.name}</div>
            {trainer.lessonsOffered === null && (
              <div className="text-sm text-neutral-10">bez omezení</div>
            )}
          </div>
        );
      })}
    </fieldset>
  );
}
