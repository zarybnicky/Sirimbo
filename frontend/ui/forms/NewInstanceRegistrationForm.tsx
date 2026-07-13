import {
  type EventInstanceTrainerFragment,
  SetEventInstanceRegistrationDocument,
} from '@/graphql/Event';
import { TextAreaElement } from '@/ui/fields/textarea';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  note: z.string().prefault(''),
  lessonCounts: z.array(z.number().min(0).prefault(0)).prefault([]),
});

type FormValues = z.infer<typeof Form>;

export function NewInstanceRegistrationForm({
  instanceId,
  enableNotes,
  personId,
  coupleId,
  label,
  lessonTrainers: allLessonTrainers,
  disabled,
  onRegistered,
}: {
  instanceId: string;
  enableNotes: boolean;
  personId: string | null;
  coupleId: string | null;
  label: string;
  lessonTrainers: EventInstanceTrainerFragment[];
  disabled: boolean;
  onRegistered: () => void;
}) {
  const [expanded, setExpanded] = React.useState(false);
  const regionId = React.useId();
  const lessonTrainers = allLessonTrainers.filter(
    (trainer) => trainer.lessonsOffered !== 0,
  );
  const hasOptions = enableNotes || lessonTrainers.length > 0;
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: {
      note: '',
      lessonCounts: lessonTrainers.map(() => 0),
    },
  });
  const setRegistration = useMutation(SetEventInstanceRegistrationDocument)[1];
  const onSubmit = useAsyncCallback(async (values: FormValues) => {
    const result = await setRegistration({
      input: {
        pInstanceId: instanceId,
        pPersonId: personId,
        pCoupleId: coupleId,
        pIsRegistered: true,
        pNote: enableNotes ? values.note : null,
        pLessonTrainerIds: lessonTrainers.map((trainer) => trainer.id),
        pLessonCounts: lessonTrainers.map((_, index) => values.lessonCounts[index] ?? 0),
      },
    });
    if (result.error) throw result.error;
    toast.success('Přihlášení proběhlo úspěšně.');
    onRegistered();
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <div className="flex items-center justify-between gap-2">
        <span>{label}</span>
        {!expanded && (
          <SubmitButton
            type={hasOptions ? 'button' : 'submit'}
            loading={onSubmit.loading}
            disabled={disabled}
            aria-expanded={hasOptions ? false : undefined}
            aria-controls={hasOptions ? regionId : undefined}
            onClick={hasOptions ? () => setExpanded(true) : undefined}
          >
            Přihlásit
          </SubmitButton>
        )}
      </div>

      {expanded && (
        <div id={regionId} className="space-y-2 rounded-md border border-neutral-4 bg-neutral-2 p-3">
          {enableNotes && (
            <TextAreaElement
              autoFocus
              control={control}
              label="Poznámky k registraci, požadavky na stravu apod."
              name="note"
            />
          )}

          {lessonTrainers.length > 0 && (
            <fieldset className="space-y-2">
              <legend>Požadavky na lekce</legend>
              {lessonTrainers.map((trainer, index) => (
                <div key={trainer.id} className="flex items-end gap-2">
                  <TextFieldElement
                    control={control}
                    name={`lessonCounts.${index}`}
                    type="number"
                    label={trainer.person?.name ?? 'Trenér'}
                    min={0}
                    max={
                      trainer.lessonsOffered === null
                        ? undefined
                        : Math.max(trainer.lessonsRemaining ?? 0, 0)
                    }
                  />
                </div>
              ))}
            </fieldset>
          )}

          <FormError error={onSubmit.error} />
          <SubmitButton loading={onSubmit.loading}>Potvrdit přihlášení</SubmitButton>
        </div>
      )}

      {!expanded && <FormError error={onSubmit.error} />}
    </form>
  );
}
