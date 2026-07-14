import {
  type EventInstanceRegistrationFragment,
  type EventInstanceTrainerFragment,
  SetEventInstanceRegistrationDocument,
} from '@/graphql/Event';
import { TextAreaElement } from '@/ui/fields/textarea';
import { FormError } from '@/ui/form';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { useConfirm } from '@/ui/Confirm';
import { zodResolver } from '@hookform/resolvers/zod';
import { Minus, Plus } from 'lucide-react';
import { useAsyncCallback } from 'react-async-hook';
import { type Control, useController, useForm } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { z } from 'zod';

const Form = z.object({
  note: z.string(),
  lessonCounts: z.array(z.number().min(0)),
});

type FormValues = z.infer<typeof Form>;

export function InstanceRegistrationForm({
  instanceId,
  enableDetails,
  enableNotes,
  personId,
  coupleId,
  registration,
  lessonTrainers: allLessonTrainers,
  onSaved,
  onCancelled,
}: {
  instanceId: string;
  enableDetails: boolean;
  enableNotes: boolean;
  personId: string | null;
  coupleId: string | null;
  registration?: EventInstanceRegistrationFragment;
  lessonTrainers: EventInstanceTrainerFragment[];
  onSaved: () => void;
  onCancelled: () => void;
}) {
  const confirm = useConfirm();
  const setRegistration = useMutation(SetEventInstanceRegistrationDocument)[1];
  const requestCounts = Object.fromEntries(
    registration?.eventLessonDemandsByRegistrationIdList.map((demand) => [
      demand.trainerId,
      demand.lessonCount,
    ]) ?? [],
  );
  const lessonTrainers = enableDetails
    ? allLessonTrainers.filter(
        (trainer) => trainer.lessonsOffered !== 0 || (requestCounts[trainer.id] ?? 0) > 0,
      )
    : [];
  const showNotes = enableDetails && (enableNotes || !!registration?.note);
  const hasFields = showNotes || lessonTrainers.length > 0;
  const { control, handleSubmit } = useForm<FormValues>({
    resolver: zodResolver(Form),
    defaultValues: {
      note: registration?.note ?? '',
      lessonCounts: lessonTrainers.map((trainer) => requestCounts[trainer.id] ?? 0),
    },
  });

  const save = useAsyncCallback(async (values: FormValues) => {
    const result = await setRegistration({
      input: {
        pInstanceId: instanceId,
        pPersonId: personId,
        pCoupleId: coupleId,
        pIsRegistered: true,
        pNote: showNotes ? values.note : null,
        pLessonTrainerIds: lessonTrainers.map((trainer) => trainer.id),
        pLessonCounts: lessonTrainers.map((_, index) => values.lessonCounts[index] ?? 0),
      },
    });
    if (result.error) throw result.error;
    toast.success(registration ? 'Přihláška upravena.' : 'Přihlášení proběhlo úspěšně.');
    onSaved();
  });

  const cancel = useAsyncCallback(async () => {
    await confirm({ description: 'Opravdu chcete zrušit přihlášku?' });
    const result = await setRegistration({
      input: {
        pInstanceId: instanceId,
        pPersonId: personId,
        pCoupleId: coupleId,
        pIsRegistered: false,
      },
    });
    if (result.error) throw result.error;
    toast.success('Přihláška zrušena.');
    onCancelled();
  });

  return (
    <form className="grid gap-3" onSubmit={handleSubmit(save.execute)}>
      <FormError error={save.error || cancel.error} />

      {showNotes && (
        <TextAreaElement
          control={control}
          label="Poznámky k registraci, požadavky na stravu apod."
          name="note"
        />
      )}

      {lessonTrainers.length > 0 && (
        <fieldset className="grid gap-2">
          <legend>Požadavky na lekce</legend>
          {lessonTrainers.map((trainer, index) => (
            <LessonRequestField
              key={trainer.id}
              control={control}
              name={`lessonCounts.${index}`}
              trainer={trainer}
              current={requestCounts[trainer.id] ?? 0}
            />
          ))}
        </fieldset>
      )}

      <div className="flex justify-between gap-2">
        {registration && (
          <SubmitButton
            type="button"
            variant="outline"
            loading={cancel.loading}
            onClick={cancel.execute}
          >
            Zrušit přihlášku
          </SubmitButton>
        )}
        {(!registration || hasFields) && (
          <SubmitButton className="ml-auto" loading={save.loading}>
            {registration ? 'Uložit' : 'Přihlásit'}
          </SubmitButton>
        )}
      </div>
    </form>
  );
}

function LessonRequestField({
  control,
  name,
  trainer,
  current,
}: {
  control: Control<FormValues>;
  name: `lessonCounts.${number}`;
  trainer: EventInstanceTrainerFragment;
  current: number;
}) {
  const { field } = useController({ control, name });
  const value = field.value ?? 0;
  const label = trainer.person?.name;
  const max =
    trainer.lessonsOffered === null
      ? null
      : current + Math.max(trainer.lessonsRemaining ?? 0, 0);

  return (
    <div className="flex flex-wrap items-center gap-2">
      <span className="grow">{label}</span>
      <div className="ml-auto flex items-center gap-2">
        {max !== null && (
          <span className="text-sm text-neutral-10">zbývá {max - value}</span>
        )}
        <button
          type="button"
          className={buttonCls({ size: 'icon', variant: 'outline' })}
          aria-label={`Snížit počet lekcí s ${label}`}
          disabled={value === 0}
          onClick={() => field.onChange(value - 1)}
        >
          <Minus />
        </button>
        <output className="px-1 tabular-nums">{value}</output>
        <button
          type="button"
          className={buttonCls({ size: 'icon', variant: 'outline' })}
          aria-label={`Zvýšit počet lekcí s ${label}`}
          disabled={max !== null && value >= max}
          onClick={() => field.onChange(value + 1)}
        >
          <Plus />
        </button>
      </div>
    </div>
  );
}
